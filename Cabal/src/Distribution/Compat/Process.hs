{-# LANGUAGE CPP #-}

#if defined(__MHS__) || defined(MHS)
-- | Portability seam for process operations.
--
-- Under GHC this module re-exports "System.Process" (plus the 'proc'
-- wrapper below).  Under MHS (MicroHs, which has neither the
-- @process@ nor the @unix@ package) processes are spawned with
-- @posix_spawnp@, reaped with @waitpid@/@WNOHANG@ polled with
-- 'threadDelay', and terminated with @kill@; see
-- "Distribution.Compat.Internal.MhsProcess".  'CreatePipe' streams
-- fall back to temporary files because MHS 'Handle's cannot be built
-- on top of file descriptors.
--
-- MicroHs defines @__MHS__@ itself; a manual @-DMHS@ flag is accepted
-- as well.  Either macro selects this branch.
--
-- Limitations of the MHS branch, relative to @process@:
--
-- * The @close_fds@, @create_group@, @detach_console@,
--   @create_new_console@, @new_session@, @child_group@, @child_user@
--   and @use_process_jobs@ fields of 'CreateProcess' are accepted but
--   ignored.  The @cwd@ and @env@ fields are honoured by temporarily
--   changing the current directory and environment around the spawn,
--   which is not safe against concurrent spawns from other threads.
-- * @UseHandle@ accepts the standard streams, 'Handle's created by
--   'newStagedInput' (or registered by an internal pipe stream), and
--   ordinary file 'Handle's (redirected via a temporary file that is
--   staged or copied back on reap, so logging to files works).  Any
--   other 'Handle' throws.
-- * 'CreatePipe' is rejected: temporary files cannot emulate blocking
--   pipes, so streaming through the public 'createProcess' API would
--   silently truncate.  Use 'captureProcess',
--   'readCreateProcessWithExitCode' or 'newStagedInput', which own the
--   stage\/spawn\/wait\/read order and are complete by construction.
module Distribution.Compat.Process
  ( CreateProcess (..)
  , CmdSpec (..)
  , StdStream (..)
  , ProcessHandle
  , proc
  , shell
  , createProcess
  , createProcess_
  , cleanupProcess
  , readCreateProcessWithExitCode
  , readProcessWithExitCode
  , captureProcess
  , StagedInput (..)
  , CapturedOutputs (..)
  , newStagedInput
  , callProcess
  , waitForProcess
  , getProcessExitCode
  , terminateProcess
  , showCommandForUser
  , createPipe
  , enableProcessJobs
  ) where

import Data.Char (isAlphaNum)
import Data.Maybe (catMaybes)
import Distribution.Compat.Directory (getTemporaryDirectory, removeFile)
import Distribution.Compat.Internal.MhsProcess
  ( MhsChild
  , MhsRedirect (..)
  , mhsJoin
  , readBytes
  , mhsLookupTemp
  , mhsRegisterTemp
  , mhsRelease
  , mhsSpawn
  , mhsTempFile
  , mhsTerminate
  , mhsTryWait
  , mhsWait
  )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Control.Exception (throwIO)
import System.Exit (ExitCode (..))
import Distribution.Compat.Internal.TempFile (openTempFile)
import System.IO (Handle, IOMode (..), hClose, hPutStr, openFile, stderr, stdin, stdout)

-- | How to start a process: a shell command line or a raw executable
-- with arguments.
data CmdSpec
  = ShellCommand String
  | RawCommand FilePath [String]
  deriving (Eq, Show)

-- | What to do with a standard stream of the child.
data StdStream
  = Inherit
  | UseHandle Handle
  | CreatePipe
  | NoStream

-- | Process creation parameters.  Only @cmdspec@, @cwd@, @env@,
-- @std_in@, @std_out@ and @std_err@ affect the MHS spawn; the
-- remaining fields are accepted and ignored.
data CreateProcess = CreateProcess
  { cmdspec :: CmdSpec
  , cwd :: Maybe FilePath
  , env :: Maybe [(String, String)]
  , std_in :: StdStream
  , std_out :: StdStream
  , std_err :: StdStream
  , close_fds :: Bool
  , create_group :: Bool
  , delegate_ctlc :: Bool
  , detach_console :: Bool
  , create_new_console :: Bool
  , new_session :: Bool
  , child_group :: Maybe Int
  , child_user :: Maybe Int
  , use_process_jobs :: Bool
  }

-- | An opaque handle to a spawned child.
newtype ProcessHandle = ProcessHandle MhsChild

-- | Build a raw process specification.  Defaults to inheriting the
-- standard streams and, as in the GHC branch, enabling @delegate_ctlc@.
proc :: FilePath -> [String] -> CreateProcess
proc path args =
  CreateProcess
    { cmdspec = RawCommand path args
    , cwd = Nothing
    , env = Nothing
    , std_in = Inherit
    , std_out = Inherit
    , std_err = Inherit
    , close_fds = False
    , create_group = False
    , delegate_ctlc = True
    , detach_console = False
    , create_new_console = False
    , new_session = False
    , child_group = Nothing
    , child_user = Nothing
    , use_process_jobs = False
    }

-- | Build a shell process specification.
shell :: String -> CreateProcess
shell command = (proc "/bin/sh" ["-c", command]) {cmdspec = ShellCommand command}

-- | Process jobs only exist on Windows; a no-op here.
enableProcessJobs :: CreateProcess -> CreateProcess
enableProcessJobs = id

-- | Resolve one standard stream for the public 'createProcess' API.
-- Returns the spawn redirection, an optional parent 'Handle', any
-- temporary file the stream owns, and any @(path, handle)@ copy to
-- perform on reap.
--
-- 'CreatePipe' is rejected outright: a temporary file cannot emulate
-- a blocking pipe, so handing one out would let callers silently
-- observe truncated output.  Internal capture paths
-- ('captureProcessOutput', 'newStagedInput') own the
-- stage\/spawn\/wait\/read order instead and never go through here.
resolveStream :: StdStream -> Bool -> IO (MhsRedirect, Maybe Handle, Maybe FilePath, [(FilePath, Handle)])
resolveStream Inherit _ = pure (MhsInherit, Nothing, Nothing, [])
resolveStream NoStream _ = pure (MhsNull, Nothing, Nothing, [])
resolveStream CreatePipe isStdin =
  ioError
    ( userError
        ( "Distribution.Compat.Process: "
            ++ (if isStdin then "std_in" else "std_out/std_err")
            ++ " CreatePipe is not supported in the MHS build"
            ++ " (temporary files cannot emulate blocking pipes); use"
            ++ " captureProcessOutput, readCreateProcessWithExitCode or"
            ++ " newStagedInput instead"
        )
    )
resolveStream (UseHandle h) isStdin = do
  mtmp <- mhsLookupTemp h
  case mtmp of
    Just tmp -> pure (MhsFile tmp, Nothing, Nothing, [])
    Nothing
      | h == stdin -> pure (MhsFd 0, Nothing, Nothing, [])
      | h == stdout -> pure (MhsFd 1, Nothing, Nothing, [])
      | h == stderr -> pure (MhsFd 2, Nothing, Nothing, [])
      | isStdin -> do
          -- Assume a regular file: stage its full contents before
          -- spawn so the child observes complete input however late
          -- it starts reading.  'readBytes' (not 'BS.hGetContents'):
          -- on a text-mode handle @c_readb@ falls back to
          -- per-character @getb_utf8@, truncating every decoded code
          -- point above 0x7F to a single byte.
          content <- readBytes h
          tmp <- mhsTempFile
          BS.writeFile tmp content
          pure (MhsFile tmp, Nothing, Just tmp, [])
      | otherwise -> do
          -- Assume a regular file (e.g. a build log): redirect the
          -- child to a temporary file and append it to the handle on
          -- reap, so logging keeps working.
          tmp <- mhsTempFile
          pure (MhsFile tmp, Nothing, Just tmp, [(tmp, h)])

-- | Start a child, returning the parent ends of any pipe streams plus
-- a handle to the child.  'CreatePipe' streams are rejected (see
-- 'resolveStream'); only 'Inherit', 'NoStream' and 'UseHandle'
-- streams are supported.
createProcess_ :: String -> CreateProcess -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess_ _name cp = do
  (redirIn, mIn, tmpIn, copyIn) <- resolveStream (std_in cp) True
  (redirOut, mOut, tmpOut, copyOut) <- resolveStream (std_out cp) False
  (redirErr, mErr, tmpErr, copyErr) <- resolveStream (std_err cp) False
  let (prog, args) = case cmdspec cp of
        RawCommand path as -> (path, as)
        ShellCommand command -> ("/bin/sh", ["-c", command])
      temps = catMaybes [tmpIn, tmpOut, tmpErr]
      copies = copyIn ++ copyOut ++ copyErr
  child <- mhsSpawn prog args (cwd cp) (env cp) redirIn redirOut redirErr temps copies
  pure (mIn, mOut, mErr, ProcessHandle child)

-- | Stage @input@ in a temporary file (locale-encoded, like
-- 'System.IO.hPutStr') and hand back a readable 'Handle' suitable
-- for 'UseHandle', plus a cleanup action that closes the handle and
-- removes the file.  The input is complete before any child spawned
-- with the handle starts reading, so there is no writer/reader race.
newStagedInput :: String -> IO (Handle, IO ())
newStagedInput input = do
  tmpDir <- getTemporaryDirectory
  (tmp, wh) <- openTempFile tmpDir "cabal-staged-input"
  hPutStr wh input
  hClose wh
  rh <- openFile tmp ReadMode
  mhsRegisterTemp rh tmp
  pure (rh, hClose rh >> removeFile tmp)

-- | Standard input staging for 'captureProcess'.
--
-- 'TextInput' is staged with a locale-encoded text write (matching
-- 'System.IO.hPutStr', never 'Data.ByteString.Lazy.Char8.pack', so
-- non-ASCII text round-trips); 'BinaryInput' is staged byte-for-byte.
data StagedInput
  = NoInput
  | TextInput String
  | BinaryInput LBS.ByteString

-- | Captured standard output of a finished child: the paths holding
-- the complete outputs plus the release action that copies
-- 'UseHandle' redirections back and removes the temporary files.
-- Read the files (with the decoding of your choice) before running
-- 'capturedRelease'; reading before the child has been reaped
-- observes partial data.
data CapturedOutputs = CapturedOutputs
  { capturedOutPath :: FilePath
  , capturedErrPath :: FilePath
  , capturedRelease :: IO ()
  }

-- | Run a command to completion, capturing its standard output and
-- error to temporary files.  Capture discipline: the input (if any)
-- is staged before spawn, and this function only returns after the
-- child has been reaped, so reading the output paths afterwards is
-- complete by construction and can never silently truncate.
--
-- The @std_in@, @std_out@ and @std_err@ fields of the 'CreateProcess'
-- are ignored (output always goes to the captured files); only the
-- command, arguments, working directory and environment are
-- honoured.  Passing 'CreatePipe' there is pointless but harmless.
captureProcess :: CreateProcess -> StagedInput -> IO (ExitCode, CapturedOutputs)
captureProcess cp staged = do
  (redirIn, inTmp) <- case staged of
    NoInput -> pure (MhsNull, Nothing)
    TextInput input -> do
      tmp <- mhsTempFile
      writeFile tmp input
      pure (MhsFile tmp, Just tmp)
    BinaryInput input -> do
      tmp <- mhsTempFile
      LBS.writeFile tmp input
      pure (MhsFile tmp, Just tmp)
  outTmp <- mhsTempFile
  errTmp <- mhsTempFile
  let (prog, args) = case cmdspec cp of
        RawCommand path as -> (path, as)
        ShellCommand command -> ("/bin/sh", ["-c", command])
      temps = catMaybes [inTmp, Just outTmp, Just errTmp]
  child <- mhsSpawn prog args (cwd cp) (env cp) redirIn (MhsFile outTmp) (MhsFile errTmp) temps []
  exit <- mhsJoin child
  pure (exit, CapturedOutputs outTmp errTmp (mhsRelease child))

-- | Start a child with the default failure context.
createProcess :: CreateProcess -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess = createProcess_ "createProcess"

-- | Release a child created by 'createProcess_': close the pipe
-- 'Handle's, reap the child (waiting if needed, so no zombies are
-- left behind) and delete its temporary files.
cleanupProcess :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO ()
cleanupProcess (mIn, mOut, mErr, ProcessHandle child) = do
  mapM_ hClose (catMaybes [mIn, mOut, mErr])
  _ <- mhsWait child
  pure ()

-- | Block until the child exits, returning its exit code.  Polls
-- @waitpid@ with @WNOHANG@; the result is cached, so repeated calls
-- are cheap.
waitForProcess :: ProcessHandle -> IO ExitCode
waitForProcess (ProcessHandle child) = mhsWait child

-- | 'System.Process.readCreateProcessWithExitCode' equivalent built
-- on 'captureProcess': the input is staged (locale-encoded, like
-- 'System.IO.hPutStr') before spawn and the outputs are read (locale
-- decoded, like 'System.IO.hGetContents') after reap, so
-- temporary-file redirection is complete by construction.
readCreateProcessWithExitCode :: CreateProcess -> String -> IO (ExitCode, String, String)
readCreateProcessWithExitCode cp input = do
  (exit, captured) <- captureProcess (cp {std_in = NoStream, std_out = NoStream, std_err = NoStream}) (TextInput input)
  out <- readFile (capturedOutPath captured)
  err <- readFile (capturedErrPath captured)
  length out `seq` length err `seq` return ()
  capturedRelease captured
  return (exit, out, err)

-- | 'System.Process.readProcessWithExitCode' equivalent built on
-- 'readCreateProcessWithExitCode'.
readProcessWithExitCode :: FilePath -> [String] -> String -> IO (ExitCode, String, String)
readProcessWithExitCode cmd args = readCreateProcessWithExitCode (proc cmd args)

-- | 'System.Process.callProcess' equivalent: inherits the standard
-- streams, waits, and throws the exit code on failure.
callProcess :: FilePath -> [String] -> IO ()
callProcess cmd args = do
  (_, _, _, ph) <- createProcess (proc cmd args)
  exit <- waitForProcess ph
  cleanupProcess (Nothing, Nothing, Nothing, ph)
  case exit of
    ExitSuccess -> return ()
    failure -> throwIO failure

-- | Non-blocking exit check.
getProcessExitCode :: ProcessHandle -> IO (Maybe ExitCode)
getProcessExitCode (ProcessHandle child) = mhsTryWait child

-- | Ask the child to terminate with @SIGTERM@.  Does not wait.
terminateProcess :: ProcessHandle -> IO ()
terminateProcess (ProcessHandle child) = mhsTerminate child

-- | Rejected in the MHS build: a temporary-file pair cannot emulate
-- a blocking pipe (see 'resolveStream').  Restructure callers to use
-- 'captureProcess', 'readCreateProcessWithExitCode' or
-- 'newStagedInput' instead.
createPipe :: IO (Handle, Handle)
createPipe =
  ioError
    ( userError
        "Distribution.Compat.Process.createPipe is not supported in the MHS build"
    )

-- | Quote a command line for display, matching "System.Process".
showCommandForUser :: FilePath -> [String] -> String
showCommandForUser cmd args = unwords (map translate (cmd : args))
  where
    translate "" = "''"
    translate str
      | all goodChar str = str
      | otherwise = '\'' : foldr escape "'" str
    escape '\'' = showString "'\\''"
    escape c = showChar c
    goodChar c = isAlphaNum c || c `elem` "-_.,/"

#else

module Distribution.Compat.Process
  ( -- * Re-exported module
    module System.Process

    -- * Redefined functions
  , proc

    -- * Additions
  , enableProcessJobs
  , captureProcess
  , StagedInput (..)
  , CapturedOutputs (..)
  ) where

import qualified Data.ByteString.Lazy as LBS
import System.Directory (getTemporaryDirectory, removeFile)
import System.Exit (ExitCode (..))
import System.IO (IOMode (..), hClose, hPutStr, openFile, openTempFile)
import System.Process hiding (proc)
import qualified System.Process as Process

#if defined(mingw32_HOST_OS) && MIN_VERSION_process(1,6,9)
import           System.IO.Unsafe (unsafePerformIO)
import           System.Win32.Info.Version (dwMajorVersion, dwMinorVersion, getVersionEx)
#endif

-------------------------------------------------------------------------------
-- enableProcessJobs
-------------------------------------------------------------------------------

#if defined(mingw32_HOST_OS) && MIN_VERSION_process(1,6,9)
-- This exception, needed to support Windows 7, could be removed when
-- the lowest GHC version cabal supports is a GHC that doesn’t support
-- Windows 7 any more.
{-# NOINLINE isWindows8OrLater #-}
isWindows8OrLater :: Bool
isWindows8OrLater = unsafePerformIO $ do
  v <- getVersionEx
  pure $ (dwMajorVersion v, dwMinorVersion v) >= (6, 2)
#endif

-- | Enable process jobs to ensure accurate determination of process completion
-- in the presence of @exec(3)@ on Windows.
--
-- Unfortunately the process job support is badly broken in @process@ releases
-- prior to 1.6.9, so we disable it in these versions, despite the fact that
-- this means we may see sporadic build failures without jobs.
--
-- On Windows 7 or before the jobs are disabled due to the fact that
-- processes on these systems can only have one job. This prevents
-- spawned process from assigning jobs to its own children. Suppose
-- process A spawns process B. The B process has a job assigned (call
-- it J1) and when it tries to spawn a new process C the C
-- automatically inherits the job. But at it also tries to assign a
-- new job J2 to C since it doesn’t have access J1. This fails on
-- Windows 7 or before.
enableProcessJobs :: CreateProcess -> CreateProcess
#if defined(mingw32_HOST_OS) && MIN_VERSION_process(1,6,9)
enableProcessJobs cp = cp {Process.use_process_jobs = isWindows8OrLater}
#else
enableProcessJobs cp = cp
#endif

-------------------------------------------------------------------------------
-- process redefinitions
-------------------------------------------------------------------------------

-- | 'System.Process.proc' with process jobs enabled when appropriate,
-- and defaulting 'delegate_ctlc' to 'True'.
proc :: FilePath -> [String] -> CreateProcess
proc path args = enableProcessJobs (Process.proc path args){Process.delegate_ctlc = True}

-- | How to provide standard input to 'captureProcess'.
data StagedInput
  = NoInput
  | TextInput String
  | BinaryInput LBS.ByteString

-- | Captured standard output of a finished child: the paths holding
-- the complete outputs plus a release action removing the temporary
-- files.  Read the files before running 'capturedRelease'.
data CapturedOutputs = CapturedOutputs
  { capturedOutPath :: FilePath
  , capturedErrPath :: FilePath
  , capturedRelease :: IO ()
  }

-- | Run a command to completion, capturing standard output and error
-- to temporary files.  The child is reaped before this returns, so
-- reading 'capturedOutPath' and 'capturedErrPath' is complete by
-- construction.  Text is staged locale-encoded (like
-- 'System.IO.hPutStr'); binary input is staged byte-for-byte.
captureProcess :: CreateProcess -> StagedInput -> IO (ExitCode, CapturedOutputs)
captureProcess cp staged = do
  tmpDir <- getTemporaryDirectory
  let stage put = do
        (p, h) <- openTempFile tmpDir "cabal-captured-in"
        put h
        hClose h
        inH <- openFile p ReadMode
        return (UseHandle inH, p)
  mIn <- case staged of
    NoInput -> pure Nothing
    TextInput s -> Just <$> stage (`hPutStr` s)
    BinaryInput b -> Just <$> stage (`LBS.hPutStr` b)
  let inStd = case mIn of
        Nothing -> NoStream
        Just (h, _) -> h
  (outTmp, outH) <- openTempFile tmpDir "cabal-captured-out"
  (errTmp, errH) <- openTempFile tmpDir "cabal-captured-err"
  hClose outH
  hClose errH
  outW <- openFile outTmp WriteMode
  errW <- openFile errTmp WriteMode
  (_, _, _, ph) <-
    Process.createProcess
      cp
        { Process.std_in = inStd
        , Process.std_out = UseHandle outW
        , Process.std_err = UseHandle errW
        }
  case mIn of
    Just (UseHandle h, _) -> hClose h
    _ -> return ()
  hClose outW
  hClose errW
  exit <- Process.waitForProcess ph
  Process.cleanupProcess (Nothing, Nothing, Nothing, ph)
  case mIn of
    Just (_, p) -> removeFile p
    Nothing -> return ()
  return (exit, CapturedOutputs outTmp errTmp (removeFile outTmp >> removeFile errTmp))
#endif
