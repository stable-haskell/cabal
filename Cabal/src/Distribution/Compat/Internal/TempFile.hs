{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}

module Distribution.Compat.Internal.TempFile
  ( openTempFile
  , openBinaryTempFile
  , openNewBinaryFile
  , createTempDirectory
  ) where

import Distribution.Compat.Exception

#ifdef __MHS__
import Data.IORef (IORef, atomicModifyIORef', newIORef)
#else
import GHC.IORef (IORef, atomicModifyIORef'_, newIORef)
#endif
import System.FilePath ((</>))
#ifdef __MHS__
import System.IO (Handle, IOMode (..), openBinaryFile, openFile)
#else
import System.IO (Handle, openBinaryTempFile, openBinaryTempFileWithDefaultPermissions, openTempFile)
#endif
import System.IO.Error (isAlreadyExistsError)
import System.IO.Unsafe (unsafePerformIO)
#ifdef __MHS__
import System.IO.Error (IOError, alreadyExistsErrorType, mkIOError)
#endif

#ifdef __MHS__
-- MicroHs lacks 'GHC.IORef.atomicModifyIORef'_'; 'Data.IORef' only has
-- the pair-returning 'atomicModifyIORef''. The old and new values are
-- reconstructed from it.
atomicModifyIORef'_ :: IORef a -> (a -> a) -> IO (a, a)
atomicModifyIORef'_ ref f =
  atomicModifyIORef' ref (\x -> let x' = f x in (x', (x, x')))
#endif

#ifdef __MHS__
import Distribution.Compat.Directory
  ( createFileExclusively
  , createPrivateDirectory
  , removeFile
  )
#else
import System.Posix.Internals (c_getpid)
import qualified System.Posix
#endif

openNewBinaryFile :: FilePath -> String -> IO (FilePath, Handle)
#ifdef __MHS__
-- No permission bits under MicroHs; same as 'openBinaryTempFile'.
openNewBinaryFile = openBinaryTempFile
#else
openNewBinaryFile = openBinaryTempFileWithDefaultPermissions
#endif

#ifdef __MHS__
-- MicroHs 'System.IO' has no temporary-file primitives; they are
-- implemented here on top of 'openFile'/'openBinaryFile' with
-- atomically-created files (see 'createFileExclusively') and
-- per-process counters (see 'tempDirectoryCounter').
openTempFile :: FilePath -> String -> IO (FilePath, Handle)
openTempFile = openTempFileWith openFile

openBinaryTempFile :: FilePath -> String -> IO (FilePath, Handle)
openBinaryTempFile = openTempFileWith openBinaryFile

openTempFileWith
  :: (FilePath -> IOMode -> IO Handle)
  -> FilePath -> String -> IO (FilePath, Handle)
openTempFileWith open dir template = do
  pid <- getTempPid
  let findTemp = do
        (counter, _) <- atomicModifyIORef'_ tempDirectoryCounter (+ 1)
        let path = dir </> template ++ "-" ++ show pid ++ show counter
        -- Atomically create the file: O_CREAT|O_EXCL fails if anything
        -- (a regular file, a directory, or a live or dangling symlink)
        -- already exists at the path, closing both the check-then-open
        -- race between processes and the symlink plant.
        created <- createFileExclusively path 0o600
        if not created
          then findTemp
          else do
            rh <- tryIO (open path ReadWriteMode)
            case rh of
              Right h -> pure (path, h)
              -- Practically unreachable (the file was just created by
              -- us): remove the empty file and propagate the failure
              -- rather than leaking it.
              Left e -> do
                _ <- tryIO (removeFile path) :: IO (Either IOError ())
                ioError e
  findTemp
#endif

-- | Process identifier seed for temporary names. Only the MHS build
-- lacks 'System.Posix.Internals.c_getpid' (which exists on all GHC
-- platforms, including Windows); there uniqueness comes from the
-- per-process counter plus atomic creation (which retries on
-- collision) rather than from a pid.
getTempPid :: IO Int
#ifdef __MHS__
getTempPid = pure 0
#else
getTempPid = fromIntegral <$> c_getpid
#endif

createTempDirectory :: FilePath -> String -> IO FilePath
createTempDirectory dir template = do
  pid <- getTempPid
  let findTempName = do
        (counter, _) <- atomicModifyIORef'_ tempDirectoryCounter (+ 1)
        let relpath = template ++ "-" ++ show pid ++ show counter
            dirpath = dir </> relpath
        r <- tryIO $ mkPrivateDir dirpath
        case r of
          Right _ -> pure relpath
          Left e
            | isAlreadyExistsError e -> findTempName
            | otherwise -> ioError e
  findTempName

tempDirectoryCounter :: IORef Word
tempDirectoryCounter = unsafePerformIO $ newIORef 0
{-# NOINLINE tempDirectoryCounter #-}

mkPrivateDir :: String -> IO ()
#ifdef __MHS__
-- Atomic with the requested mode (no chmod window).
mkPrivateDir s = do
  created <- createPrivateDirectory s 0o700
  if created
    then pure ()
    else ioError (mkIOError alreadyExistsErrorType "createDirectory" Nothing (Just s))
#else
mkPrivateDir s = System.Posix.createDirectory s 0o700
#endif
