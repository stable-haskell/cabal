{-# LANGUAGE CPP #-}

#if defined(__MHS__) || defined(MHS)
-- | Portability seam for directory operations.
--
-- Under GHC this module re-exports "System.Directory".
-- Under MHS (MicroHs, which has only a partial @System.Directory@ and
-- no @unix@ or @process@ packages) the missing operations are
-- implemented here: renames, removal, canonicalisation, timestamps,
-- symbolic links and executable lookup are built on the raw POSIX
-- queries in "Distribution.Compat.Internal.MhsPosix" (direct libc FFI
-- for @rename@/@rmdir@/@realpath@/@stat@/@utimes@/@symlink@/@readlink@,
-- plus small C helpers that hide the @struct stat@ layout), while
-- 'findExecutable' is a pure-Haskell search over @PATH@.
--
-- MicroHs defines @__MHS__@ itself; a manual @-DMHS@ flag is accepted
-- as well.  Either macro selects this branch.
module Distribution.Compat.Directory
  ( Permissions (..)
  , XdgDirectory (..)
  , addFileMode
  , canonicalizePath
  , copyFile
  , copyFileWithMetadata
  , createFileExclusively
  , createPrivateDirectory
  , copyPermissions
  , createDirectory
  , createDirectoryIfMissing
  , createDirectoryLink
  , createFileLink
  , doesDirectoryExist
  , doesFileExist
  , doesPathExist
  , emptyPermissions
  , findExecutable
  , findExecutables
  , getAppUserDataDirectory
  , getCurrentDirectory
  , getDirectoryContents
  , getFileSize
  , getHomeDirectory
  , getModificationTime
  , getPermissions
  , getSymbolicLinkTarget
  , getTemporaryDirectory
  , getXdgDirectory
  , listDirectory
  , makeAbsolute
  , pathIsSymbolicLink
  , removeDirectory
  , removeDirectoryRecursive
  , removeFile
  , removePathForcibly
  , renameDirectory
  , renameFile
  , setCurrentDirectory
  , setModificationTime
  , setPermissions
  , withCurrentDirectory
  ) where

import Control.Exception (catch)
import Control.Monad (filterM, unless)
import Data.Bits ((.&.), (.|.), complement)
import Data.Maybe (listToMaybe)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Distribution.Compat.Internal.MhsPosix
  ( MhsPathKind (..)
  , mhsChmod
  , mhsCreateExclusive
  , mhsMakeDir
  , mhsModTime
  , mhsPathKind
  , mhsPathMode
  , mhsReadlink
  , mhsRealpath
  , mhsRemoveDir
  , mhsRename
  , mhsSetModTime
  , mhsSymlink
  )
import System.Directory
  ( Permissions (..)
  , copyFile
  , createDirectory
  , emptyPermissions
  , getCurrentDirectory
  , getDirectoryContents
  , getFileSize
  , getHomeDirectory
  , getTemporaryDirectory
  , listDirectory
  , removeFile
  , setCurrentDirectory
  , withCurrentDirectory
  )
import System.Environment (lookupEnv)
import System.FilePath ((</>), isRelative, normalise, pathSeparator, splitSearchPath, takeDirectory, takeFileName)
import System.IO.Error (IOErrorType (..), alreadyExistsErrorType, doesNotExistErrorType, ioeSetErrorString, mkIOError)

-- | Swallow any 'IOError', reporting 'False'.  Used for existence and
-- permission probes where races with concurrent deletion are benign.
catchFalse :: IO Bool -> IO Bool
catchFalse io = catch io handle
  where
    handle :: IOError -> IO Bool
    handle _ = pure False

-- | XDG base directory specification.
data XdgDirectory
  = XdgData
  | XdgConfig
  | XdgCache
  | XdgState
  | XdgRuntime
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Query the kind of a path, following links.  Missing paths and
-- dangling links report 'MhsMissing'.
pathKind :: FilePath -> IO MhsPathKind
pathKind fp = mhsPathKind fp True

doesFileExist :: FilePath -> IO Bool
doesFileExist fp = catchFalse ((== MhsFile) <$> pathKind fp)

doesDirectoryExist :: FilePath -> IO Bool
doesDirectoryExist fp = catchFalse ((== MhsDir) <$> pathKind fp)

doesPathExist :: FilePath -> IO Bool
doesPathExist fp = catchFalse ((/= MhsMissing) <$> pathKind fp)

pathIsSymbolicLink :: FilePath -> IO Bool
pathIsSymbolicLink fp = catchFalse ((== MhsLink) <$> mhsPathKind fp False)

renameFile :: FilePath -> FilePath -> IO ()
renameFile = mhsRename

renameDirectory :: FilePath -> FilePath -> IO ()
renameDirectory = mhsRename

removeDirectory :: FilePath -> IO ()
removeDirectory = mhsRemoveDir

-- | Remove a file or directory tree, never following symbolic links:
-- links (including links to directories) are unlinked themselves.
-- Matches 'System.Directory.removeDirectoryRecursive': the top-level
-- target must be a real directory; a top-level symlink (even to a
-- directory) or non-directory raises an 'inappropriateTypeErrorType'
-- error, and a missing path raises 'doesNotExistErrorType'.
removeDirectoryRecursive :: FilePath -> IO ()
removeDirectoryRecursive fp = do
  kind <- mhsPathKind fp False
  case kind of
    MhsDir -> do
      entries <- listDirectory fp
      mapM_ removeEntry entries
      mhsRemoveDir fp
    MhsLink ->
      ioError
        ( ioeSetErrorString
            (mkIOError InappropriateType "removeDirectoryRecursive" Nothing (Just fp))
            "is a directory symbolic link"
        )
    MhsMissing ->
      ioError (mkIOError doesNotExistErrorType "removeDirectoryRecursive" Nothing (Just fp))
    _ ->
      ioError
        ( ioeSetErrorString
            (mkIOError InappropriateType "removeDirectoryRecursive" Nothing (Just fp))
            "not a directory"
        )
  where
    -- Single lstat classification: only real directories are recursed
    -- into; everything else (files, symlinks including links to
    -- directories, FIFOs, sockets, devices) is unlinked without
    -- following.
    removeEntry name = do
      let sub = fp </> name
      kind <- mhsPathKind sub False
      case kind of
        MhsDir -> removeDirectoryRecursive sub
        _ -> removeFile sub

-- | Remove a file or directory tree, ignoring missing paths.
-- Anything that is not a directory (files, FIFOs, sockets, devices)
-- is unlinked; symbolic links are unlinked without following them.
-- 'mhsPathKind' reports missing paths as 'MhsMissing' rather than
-- throwing, so only that case is ignored and genuine lstat failures
-- propagate, matching 'System.Directory.removePathForcibly'.
removePathForcibly :: FilePath -> IO ()
removePathForcibly fp = do
  kind <- mhsPathKind fp False
  case kind of
    MhsMissing -> pure ()
    MhsDir -> removeDirectoryRecursive fp
    _ -> removeFile fp

-- | Atomically create a regular file with the given mode bits,
-- failing when the path already exists (existing files, directories
-- and symlinks, dangling or otherwise). Returns 'True' when the file
-- was created. Use the result in a retry loop with fresh names to
-- implement race-free temporary files.
createFileExclusively :: FilePath -> Int -> IO Bool
createFileExclusively = mhsCreateExclusive

-- | Atomically create a directory with the given mode bits, failing
-- when the path already exists. Returns 'True' when the directory
-- was created. Unlike 'createDirectoryIfMissing', the mode applies
-- atomically at creation time rather than after the fact.
createPrivateDirectory :: FilePath -> Int -> IO Bool
createPrivateDirectory = mhsMakeDir

createDirectoryIfMissing :: Bool -> FilePath -> IO ()
createDirectoryIfMissing False fp = do
  created <- mhsMakeDir fp 0o777
  unless created $ do
    -- The path already exists: succeed only for directories, matching
    -- 'System.Directory' which throws 'already exists' for files.
    isDir <- doesDirectoryExist fp
    unless isDir $
      ioError (mkIOError alreadyExistsErrorType "createDirectory" Nothing (Just fp))
createDirectoryIfMissing True fp =
  mapM_ (createDirectoryIfMissing False) (rootFirst fp)
  where
    rootFirst :: FilePath -> [FilePath]
    rootFirst p = reverse (p : parents p)
    parents :: FilePath -> [FilePath]
    parents q =
      let d = takeDirectory q
       in if d == q then [] else d : parents d

-- | Resolve a path against the file system, following symbolic links.
-- Missing trailing components are tolerated: the longest existing
-- prefix is resolved and the remainder appended lexically, matching
-- 'System.Directory.canonicalizePath'.
canonicalizePath :: FilePath -> IO FilePath
canonicalizePath fp = do
  absPath <- if isRelative fp then (</> fp) <$> getCurrentDirectory else pure fp
  resolve absPath
 where
  resolve :: FilePath -> IO FilePath
  resolve p = mhsRealpath p `catch` handle p
  handle :: FilePath -> IOError -> IO FilePath
  handle p _ =
    let parent = takeDirectory p
        base = takeFileName p
     in if parent == p
          then pure (normalise p)
          else (</> base) <$> resolve parent

makeAbsolute :: FilePath -> IO FilePath
makeAbsolute p
  | isRelative p = normalise . (</> p) <$> getCurrentDirectory
  | otherwise = pure p

getSymbolicLinkTarget :: FilePath -> IO FilePath
getSymbolicLinkTarget = mhsReadlink

createFileLink :: FilePath -> FilePath -> IO ()
createFileLink = mhsSymlink

createDirectoryLink :: FilePath -> FilePath -> IO ()
createDirectoryLink = mhsSymlink

getPermissions :: FilePath -> IO Permissions
getPermissions fp = do
  -- Only owner bits are reported, matching 'System.Directory' (group
  -- and other bits do not count; @fileAccess@ root/group semantics
  -- are not reproduced).  Like 'System.Directory.getAccessPermissions'
  -- (@directory-1.3.10.0@, @System.Directory.Internal.Posix@), only
  -- directories can be searchable and only non-directories can be
  -- executable: an owner-executable regular file reports
  -- 'executable = True, searchable = False', while an
  -- owner-executable directory reports the converse.
  mode <- mhsPathMode fp
  isDir <- (== MhsDir) <$> pathKind fp
  let x = mode .&. 0o100 /= 0
  pure
    Permissions
      { readable = mode .&. 0o400 /= 0
      , writable = mode .&. 0o200 /= 0
      , executable = x && not isDir
      , searchable = x && isDir
      }

setPermissions :: FilePath -> Permissions -> IO ()
setPermissions = setPermissionsMode

-- | Helper setting the mode bits for 'setPermissions' and
-- 'copyPermissions'.
setPermissionsMode :: FilePath -> Permissions -> IO ()
setPermissionsMode fp p = do
  mode <- mhsPathMode fp
  -- Like 'System.Directory.setAccessPermissions'
  -- (@directory-1.3.10.0@): the owner-execute bit is set when either
  -- 'executable' or 'searchable' is requested, regardless of file kind;
  -- 'getPermissions' then reports it back gated on 'isDir'.
  let execBit = executable p || searchable p
      ownBits =
        (if readable p then 0o400 else 0)
          .|. (if writable p then 0o200 else 0)
          .|. (if execBit then 0o100 else 0)
  mhsChmod fp ((mode .&. complement 0o700) .|. ownBits)

copyPermissions :: FilePath -> FilePath -> IO ()
copyPermissions src dst = do
  -- Transfer the full permission bits (including group/other and
  -- set-uid/set-gid/sticky), matching 'System.Directory', which
  -- copies the whole mode rather than just the owner bits.
  srcMode <- mhsPathMode src
  dstMode <- mhsPathMode dst
  mhsChmod dst ((dstMode .&. complement 0o7777) .|. (srcMode .&. 0o7777))

-- | OR the given permission bits into a path's mode, preserving
-- file-type bits and all other bits. Used to implement the
-- @setFile*@ family where @unix@ is unavailable.
addFileMode :: FilePath -> Int -> IO ()
addFileMode fp bits = do
  mode <- mhsPathMode fp
  mhsChmod fp (mode .|. (bits .&. 0o7777))

getModificationTime :: FilePath -> IO UTCTime
getModificationTime fp = do
  (sec, nsec) <- mhsModTime fp
  pure (posixSecondsToUTCTime (fromIntegral sec + fromIntegral nsec / 1000000000))

setModificationTime :: FilePath -> UTCTime -> IO ()
setModificationTime fp t = do
  let posix = utcTimeToPOSIXSeconds t
      sec = floor posix :: Integer
  mhsSetModTime fp sec (floor ((posix - fromIntegral sec) * 1000000000))

copyFileWithMetadata :: FilePath -> FilePath -> IO ()
copyFileWithMetadata src dst = do
  copyFile src dst
  copyPermissions src dst
  getModificationTime src >>= setModificationTime dst

-- | Is the path an executable regular file?  Never throws.
isExecutableFile :: FilePath -> IO Bool
isExecutableFile fp =
  catchFalse $ do
    kind <- pathKind fp
    if kind /= MhsFile
      then pure False
      else executable <$> getPermissions fp

-- | Find an executable by name, searching @PATH@.  A name containing
-- a path separator is checked directly instead.
findExecutables :: String -> IO [FilePath]
findExecutables name
  | pathSeparator `elem` name = do
      ok <- isExecutableFile name
      pure [name | ok]
  | otherwise = do
      mpath <- lookupEnv "PATH"
      let dirs = maybe [] splitSearchPath mpath
      filterM isExecutableFile [dir </> name | dir <- dirs]

findExecutable :: String -> IO (Maybe FilePath)
findExecutable name = listToMaybe <$> findExecutables name

xdgOrHome :: String -> FilePath -> IO FilePath
xdgOrHome var defrel = do
  mdir <- lookupEnv var
  case mdir of
    Just d | not (null d) -> pure d
    _ -> (</> defrel) <$> getHomeDirectory

getXdgDirectory :: XdgDirectory -> FilePath -> IO FilePath
getXdgDirectory XdgData app = (</> app) <$> xdgOrHome "XDG_DATA_HOME" (".local" </> "share")
getXdgDirectory XdgConfig app = (</> app) <$> xdgOrHome "XDG_CONFIG_HOME" ".config"
getXdgDirectory XdgCache app = (</> app) <$> xdgOrHome "XDG_CACHE_HOME" ".cache"
getXdgDirectory XdgState app = (</> app) <$> xdgOrHome "XDG_STATE_HOME" (".local" </> "state")
getXdgDirectory XdgRuntime app = do
  mdir <- lookupEnv "XDG_RUNTIME_DIR"
  case mdir of
    Just d | not (null d) -> pure (d </> app)
    _ -> ioError (userError "getXdgDirectory: XDG_RUNTIME_DIR is not set")

getAppUserDataDirectory :: FilePath -> IO FilePath
getAppUserDataDirectory app = (</> ('.' : app)) <$> getHomeDirectory

#else

-- | GHC-only re-export shim for "System.Directory".
--
-- In-tree code should import this module instead of "System.Directory"
-- directly, so that alternative backends can be provided behind this
-- seam in a later phase. Under GHC this module is exactly
-- "System.Directory".
module Distribution.Compat.Directory
  ( module System.Directory
  ) where

import System.Directory
#endif
