{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}

{- HLINT ignore "Use fewer imports" -}

module Distribution.Compat.CopyFile
  ( copyFile
  , copyFileChanged
  , filesEqual
  , copyOrdinaryFile
  , copyExecutableFile
  , setFileOrdinary
  , setFileExecutable
  , setDirOrdinary
  ) where

import Distribution.Compat.Prelude
import Prelude ()

#ifdef __MHS__
-- Strict bytes only: lazy 'BSL.hGetContents' needs 'hGetSome',
-- which MicroHs does not implement.
import qualified Data.ByteString as BS
#else
import qualified Data.ByteString.Lazy as BSL
#endif
import Distribution.Compat.Directory
  ( doesFileExist
  )
import System.IO
  ( IOMode (ReadMode)
  , hFileSize
  , withBinaryFile
  )

#if !defined(mingw32_HOST_OS) && !defined(__MHS__) && !defined(MHS)
-- MicroHs has no @unix@ package ('System.Posix.*' are absent), so the
-- MHS build takes the fallback branch below, like Windows.
import Control.Exception
         ( bracketOnError )
import Data.Bits
         ( (.|.) )
import System.IO.Error
         ( ioeSetLocation, isAlreadyExistsError, tryIOError )
import Distribution.Compat.Directory
         ( renameFile, removeFile )
import System.FilePath
         ( takeDirectory, (</>) )
import System.IO
         ( Handle, hClose, hGetBuf, hPutBuf )
import Foreign
         ( allocaBytes )

import System.Posix.IO
         ( OpenFileFlags (cloexec, creat, exclusive), OpenMode (WriteOnly)
         , defaultFileFlags, fdToHandle, openFd )
import System.Posix.Process
         ( getProcessID )
import System.Posix.Types
         ( FileMode )
import System.Posix.Files
         ( getFileStatus, fileMode, setFileMode )

#elif defined(__MHS__) || defined(MHS)

import Control.Exception
         ( bracketOnError )
import System.IO.Error
         ( ioeSetLocation )
import Distribution.Compat.Directory
  ( addFileMode
  , renameFile
  , removeFile
  )
import Distribution.Compat.Internal.TempFile
  ( openBinaryTempFile )
import System.FilePath
  ( takeDirectory )
import System.IO
  ( hClose )

#else /* else mingw32_HOST_OS */

import Distribution.Compat.Directory
  ( copyFileWithMetadata )

#endif /* mingw32_HOST_OS */

copyOrdinaryFile, copyExecutableFile :: FilePath -> FilePath -> IO ()
copyOrdinaryFile src dest = copyFile src dest >> setFileOrdinary dest
copyExecutableFile src dest = copyFile src dest >> setFileExecutable dest

setFileOrdinary, setFileExecutable, setDirOrdinary :: FilePath -> IO ()
#if !defined(mingw32_HOST_OS) && !defined(__MHS__) && !defined(MHS)
-- When running with a restrictive UMASK such as 0077 we still want to
-- install files and directories that are accessible to other users.
setFileOrdinary   path = addFileMode path 0o644 -- file perms -rw-r--r--
setFileExecutable path = addFileMode path 0o755 -- file perms -rwxr-xr-x

addFileMode :: FilePath -> FileMode -> IO ()
addFileMode name m = do
  o <- fileMode <$> getFileStatus name
  setFileMode name (m .|. o)
#elif defined(__MHS__) || defined(MHS)
-- MHS lacks @unix@, but the Compat.Directory POSIX seam provides the
-- same OR-ing primitive over its own FFI, so installed files and
-- executables get the same permissions as on GHC.
setFileOrdinary   path = addFileMode path 0o644 -- file perms -rw-r--r--
setFileExecutable path = addFileMode path 0o755 -- file perms -rwxr-xr-x
#else
setFileOrdinary   _ = return ()
setFileExecutable _ = return ()
#endif
-- This happens to be true on Unix and currently on Windows too:
setDirOrdinary = setFileExecutable

-- | Copies a file to a new destination.
-- Often you should use `copyFileChanged` instead.
copyFile :: FilePath -> FilePath -> IO ()
#if !defined(mingw32_HOST_OS) && !defined(__MHS__) && !defined(MHS)
copyFile fromFPath toFPath =
  copy
    `catchIO` (\ioe -> throwIO (ioeSetLocation ioe "copyFile"))
  where
      copy = withBinaryFile fromFPath ReadMode $ \hFrom ->
             bracketOnError openTmp cleanTmp $ \(tmpFPath, hTmp) ->
             do allocaBytes bufferSize $ copyContents hFrom hTmp
                hClose hTmp
                renameFile tmpFPath toFPath
      openTmp = openCopyTarget (takeDirectory toFPath)
      cleanTmp (tmpFPath, hTmp) = do
        hClose hTmp          `catchIO` \_ -> return ()
        removeFile tmpFPath  `catchIO` \_ -> return ()
      bufferSize = 4096

      copyContents hFrom hTo buffer = do
              count <- hGetBuf hFrom buffer bufferSize
              when (count > 0) $ do
                      hPutBuf hTo buffer count
                      copyContents hFrom hTo buffer

-- | Create the temporary file that 'copyFile' writes to, with @O_CLOEXEC@ set
-- from the start. 'System.IO.openBinaryTempFile' cannot ask for that flag.
--
-- cabal-install runs this library in-process, on several threads. A process
-- that another thread starts while this file is open would inherit the
-- descriptor and keep it for as long as it runs, and the kernel refuses to
-- execute a file that anything holds open for writing. An executable we have
-- just installed would then fail with @ETXTBSY@, even after we close and rename
-- it.
openCopyTarget :: FilePath -> IO (FilePath, Handle)
openCopyTarget dir = getProcessID >>= \pid -> go (show pid) (0 :: Int)
  where
    go pid n = do
      let path = dir </> (".copyFile" ++ pid ++ "-" ++ show n ++ ".tmp")
      res <- tryIOError (openFd path WriteOnly flags)
      case res of
        Right fd -> (,) path <$> fdToHandle fd
        Left ioe
          | isAlreadyExistsError ioe -> go pid (n + 1)
          | otherwise -> ioError ioe
    flags = defaultFileFlags{creat = Just 0o600, exclusive = True, cloexec = True}
#elif defined(__MHS__) || defined(MHS)
-- Atomic copy matching the POSIX branch above: stage the content in
-- an exclusively-created temporary file in the destination directory
-- (via 'openBinaryTempFile', which uses O_CREAT|O_EXCL so symlinks
-- are never followed), then atomically rename over the destination.
-- Renaming replaces a destination symlink instead of following it,
-- and never leaves a truncated destination behind.
-- Content is copied with strict 'Data.ByteString' handles: both
-- 'BS.readFile' (via 'openBinaryFile' plus raw 'c_readb') and 'BS.hPut'
-- (via raw 'c_writeb') bypass the UTF-8 transducer that text-mode
-- 'System.IO' handles wrap around every handle, so arbitrary bytes
-- round-trip unchanged. (Lazy 'BSL.hGetContents' is unusable here:
-- it needs 'hGetSome', which MicroHs does not implement.)
copyFile fromFPath toFPath =
  copy
    `catchIO` (\ioe -> throwIO (ioeSetLocation ioe "copyFile"))
  where
      copy = bracketOnError openTmp cleanTmp $ \(tmpFPath, hTmp) -> do
               content <- BS.readFile fromFPath
               BS.hPut hTmp content
               hClose hTmp
               renameFile tmpFPath toFPath
      openTmp = openBinaryTempFile (takeDirectory toFPath) ".copyFile.tmp"
      cleanTmp (tmpFPath, hTmp) = do
        hClose hTmp          `catchIO` \_ -> return ()
        removeFile tmpFPath  `catchIO` \_ -> return ()
#else
copyFile = copyFileWithMetadata
#endif /* mingw32_HOST_OS */

-- | Like `copyFile`, but does not touch the target if source and destination
-- are already byte-identical. This is recommended as it is useful for
-- time-stamp based recompilation avoidance.
copyFileChanged :: FilePath -> FilePath -> IO ()
copyFileChanged src dest = do
  equal <- filesEqual src dest
  unless equal $ copyFile src dest

-- | Checks if two files are byte-identical.
-- Returns False if either of the files do not exist or if files
-- are of different size.
filesEqual :: FilePath -> FilePath -> IO Bool
filesEqual f1 f2 = do
  ex1 <- doesFileExist f1
  ex2 <- doesFileExist f2
  if not (ex1 && ex2)
    then return False
    else withBinaryFile f1 ReadMode $ \h1 ->
      withBinaryFile f2 ReadMode $ \h2 -> do
        s1 <- hFileSize h1
        s2 <- hFileSize h2
        if s1 /= s2
          then return False
          else do
#ifdef __MHS__
            -- Strict bytes: lazy 'BSL.hGetContents' needs 'hGetSome',
            -- which MicroHs does not implement.
            c1 <- BS.hGetContents h1
            c2 <- BS.hGetContents h2
#else
            c1 <- BSL.hGetContents h1
            c2 <- BSL.hGetContents h2
#endif
            return $! c1 == c2
