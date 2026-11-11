{-# LANGUAGE CPP #-}

-- | Low-level POSIX file queries for the MHS (MicroHs) build.
--
-- Under GHC this module reimplements the same interface on top of
-- "System.Directory" (plus "Data.Time" for timestamps), so the GHC
-- build behaves exactly as before.  Under MHS the operations are
-- implemented with direct libc FFI ("rename", "rmdir", "realpath",
-- "symlink", "readlink", "mkdir", "chmod") plus the small C helpers
-- in @cbits/mhs_posix.c@ that hide the platform-specific
-- @struct stat@ layout.
--
-- This module is shared plumbing for "Distribution.Compat.Directory";
-- it is not part of the public API.
module Distribution.Compat.Internal.MhsPosix
  ( MhsPathKind (..)
  , mhsPathKind
  , mhsPathMode
  , mhsChmod
  , mhsRename
  , mhsRemoveDir
  , mhsMakeDir
  , mhsCreateExclusive
  , mhsRealpath
  , mhsSymlink
  , mhsReadlink
  , mhsModTime
  , mhsSetModTime
  ) where

#if defined(__MHS__) || defined(MHS)

import Foreign.C.Error (eEXIST, getErrno, throwErrnoIfMinus1_, throwErrnoIfNull, throwErrnoPath)
import Foreign.C.String (CString, peekCAString, peekCAStringLen, withCAString)
import Foreign.C.Types (CChar, CInt, CLong, CULong)
import Foreign.Marshal.Alloc (alloca, allocaBytes, free)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)

-- | Classification of a path, see @mhs_path_kind@ in @cbits/mhs_posix.c@.
data MhsPathKind
  = MhsMissing
  | MhsFile
  | MhsDir
  | MhsLink
  | MhsOther
  deriving (Eq, Ord, Show)

foreign import ccall "mhs_posix.h mhs_path_kind" c_path_kind
  :: CString -> CInt -> IO CInt
foreign import ccall "mhs_posix.h mhs_path_mode" c_path_mode
  :: CString -> Ptr CLong -> IO CInt
foreign import ccall "mhs_posix.h mhs_path_mtime" c_path_mtime
  :: CString -> Ptr CLong -> Ptr CLong -> IO CInt
foreign import ccall "mhs_posix.h mhs_set_mtime" c_set_mtime
  :: CString -> CLong -> CLong -> IO CInt
foreign import ccall "mhs_posix.h rename" c_rename
  :: CString -> CString -> IO CInt
foreign import ccall "mhs_posix.h rmdir" c_rmdir
  :: CString -> IO CInt
foreign import ccall "mhs_posix.h mkdir" c_mkdir
  :: CString -> CInt -> IO CInt
foreign import ccall "mhs_posix.h mhs_create_exclusive" c_createExclusive
  :: CString -> CInt -> IO CInt
foreign import ccall "mhs_posix.h chmod" c_chmod
  :: CString -> CInt -> IO CInt
foreign import ccall "mhs_posix.h realpath" c_realpath
  :: CString -> Ptr CChar -> IO CString
foreign import ccall "mhs_posix.h symlink" c_symlink
  :: CString -> CString -> IO CInt
foreign import ccall "mhs_posix.h readlink" c_readlink
  :: CString -> Ptr CChar -> CULong -> IO CLong

-- | Classify a path.  When the second argument is 'True', symbolic
-- links are followed (like @stat@); otherwise the link itself is
-- reported (like @lstat@).  Missing paths report 'MhsMissing' rather
-- than throwing; other errors throw.
mhsPathKind :: FilePath -> Bool -> IO MhsPathKind
mhsPathKind fp follow =
  withCAString fp $ \cfp -> do
    r <- c_path_kind cfp (if follow then 1 else 0)
    case r of
      0 -> pure MhsMissing
      1 -> pure MhsFile
      2 -> pure MhsDir
      3 -> pure MhsLink
      _ ->
        if r > 0
          then pure MhsOther
          else throwErrnoPath "mhsPathKind" fp

-- | Raw permission bits of a path (following links), as for @st_mode@.
-- Throws when the path does not exist.
mhsPathMode :: FilePath -> IO Int
mhsPathMode fp =
  withCAString fp $ \cfp ->
    alloca $ \pmode -> do
      throwErrnoIfMinus1_ "mhsPathMode" (c_path_mode cfp pmode)
      fromIntegral <$> peek pmode

-- | Set raw permission bits, as for @chmod@.
mhsChmod :: FilePath -> Int -> IO ()
mhsChmod fp mode =
  withCAString fp $ \cfp ->
    throwErrnoIfMinus1_ "mhsChmod" (c_chmod cfp (fromIntegral mode))

-- | Rename a file or directory, as for @rename@.
mhsRename :: FilePath -> FilePath -> IO ()
mhsRename old new =
  withCAString old $ \cold ->
    withCAString new $ \cnew ->
      throwErrnoIfMinus1_ "mhsRename" (c_rename cold cnew)

-- | Remove an empty directory, as for @rmdir@.
mhsRemoveDir :: FilePath -> IO ()
mhsRemoveDir fp =
  withCAString fp $ \cfp ->
    throwErrnoIfMinus1_ "mhsRemoveDir" (c_rmdir cfp)

-- | Create a directory with the given mode, as for @mkdir@.
-- Returns 'True' when the directory was created and 'False' when it
-- already existed; other errors throw.
mhsMakeDir :: FilePath -> Int -> IO Bool
mhsMakeDir fp mode =
  withCAString fp $ \cfp -> do
    r <- c_mkdir cfp (fromIntegral mode)
    if r == 0
      then pure True
      else do
        e <- getErrno
        if e == eEXIST
          then pure False
          else throwErrnoPath "mhsMakeDir" fp

-- | Atomically create a regular file, failing when the path already
-- exists (including dangling symlinks, which @O_EXCL@ does not
-- follow). Returns 'True' when the file was created and 'False' when
-- it already exists; other errors throw.
mhsCreateExclusive :: FilePath -> Int -> IO Bool
mhsCreateExclusive fp mode =
  withCAString fp $ \cfp -> do
    r <- c_createExclusive cfp (fromIntegral mode)
    if r == 0
      then pure True
      else
        if r == 1
          then pure False
          else throwErrnoPath "mhsCreateExclusive" fp

-- | Canonicalise a path, resolving symbolic links, as for
-- @realpath@.  Throws when the path does not exist.
mhsRealpath :: FilePath -> IO FilePath
mhsRealpath fp =
  withCAString fp $ \cfp -> do
    rp <- throwErrnoIfNull "mhsRealpath" (c_realpath cfp nullPtr)
    s <- peekCAString rp
    free rp
    pure s

-- | Create a symbolic link, as for @symlink@.
mhsSymlink :: FilePath -> FilePath -> IO ()
mhsSymlink target link =
  withCAString target $ \ctarget ->
    withCAString link $ \clink ->
      throwErrnoIfMinus1_ "mhsSymlink" (c_symlink ctarget clink)

-- | Read a symbolic link, as for @readlink@.
mhsReadlink :: FilePath -> IO FilePath
mhsReadlink fp =
  withCAString fp $ \cfp -> go cfp 256
  where
    go :: CString -> Int -> IO FilePath
    go cfp size =
      allocaBytes size $ \buf -> do
        r <- c_readlink cfp buf (fromIntegral size)
        if r < 0
          then throwErrnoPath "mhsReadlink" fp
          else
            if fromIntegral r < size
              then peekCAStringLen (buf, fromIntegral r)
              else go cfp (size * 2)

-- | Modification time of a path (following links) as seconds plus
-- nanoseconds since the epoch.  Throws when the path does not exist.
mhsModTime :: FilePath -> IO (Integer, Int)
mhsModTime fp =
  withCAString fp $ \cfp ->
    alloca $ \psec ->
      alloca $ \pnsec -> do
        throwErrnoIfMinus1_ "mhsModTime" (c_path_mtime cfp psec pnsec)
        sec <- peek psec
        nsec <- peek pnsec
        pure (toInteger sec, fromIntegral nsec)

-- | Set the modification time of a path (following links) from seconds
-- plus nanoseconds since the epoch.
mhsSetModTime :: FilePath -> Integer -> Int -> IO ()
mhsSetModTime fp sec nsec =
  withCAString fp $ \cfp ->
    throwErrnoIfMinus1_ "mhsSetModTime" (c_set_mtime cfp (fromInteger sec) (fromIntegral nsec))

#else

-- GHC fallback: reimplement the same interface on top of
-- "System.Directory" and "Data.Time".  This branch only runs under
-- GHC ("Distribution.Compat.Directory" keeps calling "System.Directory"
-- directly there), so it is a safety net for direct users of this
-- module rather than behaviour-critical code.
import Data.Bits ((.&.), (.|.))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import System.IO (writeFile)
import System.IO.Error (catchIOError, isAlreadyExistsError)
import System.Directory
  ( Permissions (..)
  , canonicalizePath
  , createDirectory
  , createFileLink
  , doesDirectoryExist
  , doesFileExist
  , getModificationTime
  , getPermissions
  , getSymbolicLinkTarget
  , pathIsSymbolicLink
  , removeDirectory
  , renameFile
  , setModificationTime
  , setPermissions
  )

-- | Classification of a path.
data MhsPathKind
  = MhsMissing
  | MhsFile
  | MhsDir
  | MhsLink
  | MhsOther
  deriving (Eq, Ord, Show)

-- | Classify a path, following links when asked.
mhsPathKind :: FilePath -> Bool -> IO MhsPathKind
mhsPathKind fp follow = do
  isLink <- if follow then pure False else pathIsSymbolicLink fp
  if isLink
    then pure MhsLink
    else do
      isFile <- doesFileExist fp
      if isFile
        then pure MhsFile
        else do
          isDir <- doesDirectoryExist fp
          pure (if isDir then MhsDir else MhsMissing)

-- | Approximate raw permission bits from 'getPermissions'.
mhsPathMode :: FilePath -> IO Int
mhsPathMode fp = do
  p <- getPermissions fp
  pure $
    (if readable p then 0o444 else 0)
      .|. (if writable p then 0o222 else 0)
      .|. (if executable p then 0o111 else 0)

-- | Set permission bits (owner bits only, as for 'setPermissions').
mhsChmod :: FilePath -> Int -> IO ()
mhsChmod fp mode = do
  p <- getPermissions fp
  setPermissions
    fp
    p
      { readable = mode .&. 0o444 /= 0
      , writable = mode .&. 0o222 /= 0
      , executable = mode .&. 0o111 /= 0
      }

-- | Rename a file or directory.
mhsRename :: FilePath -> FilePath -> IO ()
mhsRename = renameFile

-- | Remove an empty directory.
mhsRemoveDir :: FilePath -> IO ()
mhsRemoveDir = removeDirectory

-- | Create a directory; 'False' when it already existed.
mhsMakeDir :: FilePath -> Int -> IO Bool
mhsMakeDir fp _mode =
  catchIOError
    (createDirectory fp >> pure True)
    (\e -> if isAlreadyExistsError e then pure False else ioError e)

-- | Best-effort 'mhsCreateExclusive' for GHC builds: GHC's own file
-- opening has no 'O_EXCL' mode without @unix@, and this helper is only
-- used by the MHS-only creation paths. The existence check plus the
-- caller's retry loop close the practical collision window.
mhsCreateExclusive :: FilePath -> Int -> IO Bool
mhsCreateExclusive fp _mode = do
  exists <- doesFileExist fp
  if exists
    then pure False
    else writeFile fp "" >> pure True

-- | Canonicalise a path, resolving symbolic links.
mhsRealpath :: FilePath -> IO FilePath
mhsRealpath = canonicalizePath

-- | Create a symbolic link.
mhsSymlink :: FilePath -> FilePath -> IO ()
mhsSymlink = createFileLink

-- | Read a symbolic link.
mhsReadlink :: FilePath -> IO FilePath
mhsReadlink = getSymbolicLinkTarget

-- | Modification time as seconds plus nanoseconds since the epoch.
mhsModTime :: FilePath -> IO (Integer, Int)
mhsModTime fp = do
  t <- utcTimeToPOSIXSeconds <$> getModificationTime fp
  let s = floor t :: Integer
  pure (s, floor ((t - fromIntegral s) * 1000000000))

-- | Set the modification time from seconds plus nanoseconds.
mhsSetModTime :: FilePath -> Integer -> Int -> IO ()
mhsSetModTime fp sec nsec =
  setModificationTime fp (posixSecondsToUTCTime (fromIntegral sec + fromIntegral nsec / 1000000000))

#endif
