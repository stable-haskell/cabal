{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Focused subprocess regression suite for the MHS process backend.
--
-- Compiles and runs under both GHC (reference semantics, via
-- 'System.Process') and MicroHs (via the MHS branch of
-- "Distribution.Compat.Process"), asserting identical observable
-- behaviour:
--
-- * T0 compiler identity (which backend under test)
-- * T1 delayed writer child, read after reap: complete output
-- * T2 non-ASCII stdin/stdout round-trip preserves locale encoding
-- * T3 @env = Just ...@ replaces the environment (removal, restore)
-- * T3b parent environment unchanged after replaced spawn
-- * T3c empty env + parent-PATH lookup
-- * T3d malformed entries pass through
-- * T4 regular-file 'UseHandle' for stdin and stdout
-- * T4b UTF-8 multibyte round-trip through text- and binary-mode
--   regular-file 'UseHandle's (no double-encoding, no truncation)
-- * T5 exit codes and stdout/stderr separation
-- * T6 public 'CreatePipe' is rejected under MHS, works under GHC
-- * T7 failing output handle surfaces an error (no silent success)
module Main where

import Distribution.Compat.Directory (getTemporaryDirectory, removeFile)
import Distribution.Compat.Process qualified as Process

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.List (isInfixOf)
import Data.Maybe (catMaybes)
import System.Environment (getEnvironment, lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (..), exitFailure, exitSuccess)
import System.IO (IOMode (..), hClose, hFlush, hGetContents, hPutStr, openBinaryFile, openFile, openTempFile, stdout)

backend :: String
#ifdef __MHS__
backend = "mhs"
#else
backend = "ghc"
#endif

type Failure = String

mark :: String -> IO ()
mark s = putStrLn s >> hFlush stdout

check :: String -> Bool -> IO (Maybe Failure)
check name True = mark ("ok   " ++ name) >> return Nothing
check name False = mark ("FAIL " ++ name) >> return (Just name)

-- T1: child writes after a delay; parent reads only after reap.
tDelayedWriter :: IO (Maybe Failure)
tDelayedWriter = do
  (exit, captured) <-
    Process.captureProcess
      (Process.proc "sh" ["-c", "sleep 1; echo out; echo err >&2"])
      Process.NoInput
  out <- readFile (Process.capturedOutPath captured)
  err <- readFile (Process.capturedErrPath captured)
  _ <- evaluate (length out + length err)
  Process.capturedRelease captured
  check "T1 delayed writer captured completely" (exit == ExitSuccess && out == "out\n" && err == "err\n")

-- T2: non-ASCII round-trip through a cat child.
tNonAscii :: IO (Maybe Failure)
tNonAscii = do
  locale <- lookupEnv "LC_ALL"
  mark ("info locale LC_ALL=" ++ show locale)
  let input = "h\xe9llo w\xf6rld \x2713\n"
  (exit, captured) <-
    Process.captureProcess
      (Process.proc "cat" [])
      (Process.TextInput input)
  out <- readFile (Process.capturedOutPath captured)
  _ <- evaluate (length out)
  Process.capturedRelease captured
  check "T2 non-ASCII round-trip" (exit == ExitSuccess && out == input)

-- T3: env replacement removes unmentioned variables, then restores.
tEnvRemoval :: IO (Maybe Failure)
tEnvRemoval = do
  setEnv "MHS_TEST_PARENT_SENTINEL" "1"
  before <- getEnvironment
  (exit, captured) <-
    Process.captureProcess
      (Process.proc "env" [])
        { Process.env = Just [("MHS_TEST_ONLY", "present")]
        }
      Process.NoInput
  out <- readFile (Process.capturedOutPath captured)
  _ <- evaluate (length out)
  Process.capturedRelease captured
  after <- getEnvironment
  unsetEnv "MHS_TEST_PARENT_SENTINEL"
  check
    "T3 env replacement and restore"
    ( exit == ExitSuccess
        && "MHS_TEST_ONLY=present\n" `isInfixOf` out
        && not ("MHS_TEST_PARENT_SENTINEL=1" `isInfixOf` out)
        && lookup "MHS_TEST_PARENT_SENTINEL" after == Just "1"
        && length before == length after
    )

-- T3b: the parent environment is unchanged by a replaced-env spawn.
tAcquireRollback :: IO (Maybe Failure)
tAcquireRollback = do
  before <- getEnvironment
  _ <- evaluate (length before)
  (exit, captured) <-
    Process.captureProcess
      (Process.proc "env" [])
        { Process.env = Just []
        }
      Process.NoInput
  out <- readFile (Process.capturedOutPath captured)
  _ <- evaluate (length out)
  Process.capturedRelease captured
  after <- getEnvironment
  check "T3b parent env unchanged by replaced spawn" (exit == ExitSuccess && before == after)

-- T3c: empty env with a relative program name: lookup still uses the
-- parent PATH while the child sees an empty environment.
tEmptyEnvLookup :: IO (Maybe Failure)
tEmptyEnvLookup = do
  (exit1, cap1) <-
    Process.captureProcess
      (Process.proc "echo" ["hi"])
        { Process.env = Just []
        }
      Process.NoInput
  out1 <- readFile (Process.capturedOutPath cap1)
  _ <- evaluate (length out1)
  Process.capturedRelease cap1
  (exit2, cap2) <-
    Process.captureProcess
      (Process.proc "env" [])
        { Process.env = Just []
        }
      Process.NoInput
  out2 <- readFile (Process.capturedOutPath cap2)
  _ <- evaluate (length out2)
  Process.capturedRelease cap2
  check "T3c empty env + parent-PATH lookup" (exit1 == ExitSuccess && out1 == "hi\n" && exit2 == ExitSuccess && out2 == "")

-- T3d: values containing '=' and empty values pass through to the
-- child unchanged (matching System.Process.createProcess).
tMalformedEnv :: IO (Maybe Failure)
tMalformedEnv = do
  (exit, captured) <-
    Process.captureProcess
      (Process.proc "env" [])
        { Process.env = Just [("MHS_K1", "a=b"), ("MHS_K2", "")]
        }
      Process.NoInput
  out <- readFile (Process.capturedOutPath captured)
  _ <- evaluate (length out)
  Process.capturedRelease captured
  check "T3d special env values pass through" (exit == ExitSuccess && "MHS_K1=a=b\n" `isInfixOf` out && "MHS_K2=\n" `isInfixOf` out)

-- T4: regular files as UseHandle stdin and stdout.
tFileHandles :: IO (Maybe Failure)
tFileHandles = do
  tmpDir <- getTemporaryDirectory
  (inTmp, inH) <- openTempFile tmpDir "t4-in"
  hPutStr inH "via-file\n"
  hClose inH
  outRes <- try $ do
    inH2 <- openFile inTmp ReadMode
    (logTmp, logH) <- openTempFile tmpDir "t4-log"
    (_, _, _, ph) <-
      Process.createProcess
        (Process.proc "cat" [])
          { Process.std_in = Process.UseHandle inH2
          , Process.std_out = Process.UseHandle logH
          , Process.std_err = Process.Inherit
          }
    hClose inH2
    exit <- Process.waitForProcess ph
    Process.cleanupProcess (Nothing, Nothing, Nothing, ph)
    hClose logH
    content <- readFile logTmp
    _ <- evaluate (length content)
    removeFile logTmp
    return (exit, content)
  removeFile inTmp
  case outRes of
    Left (e :: SomeException) -> do
      putStrLn ("info T4 raised: " ++ show e)
      check "T4 file handles (no exception)" False
    Right (exit, content) ->
      check "T4 file handles round-trip" (exit == ExitSuccess && content == "via-file\n")

-- T4b: UTF-8 multibyte content round-trips byte-identically through
-- text-mode and binary-mode regular-file 'UseHandle's, for both the
-- output copy-back and the stdin staging paths.  The bytes are
-- spelled out explicitly so the test does not depend on the locale.
-- ("caf\xc3\xa9 \xe2\x80\x9cquote\xe2\x80\x9d \xe2\x82\xac\n",
-- i.e. @"caf\xe9 \x201cquote\x201d \x20ac\n"@.)
tUtf8FileHandles :: IO (Maybe Failure)
tUtf8FileHandles = do
  tmpDir <- getTemporaryDirectory
  let want =
        BS.pack
          [ 0x63
          , 0x61
          , 0x66
          , 0xC3
          , 0xA9
          , 0x20
          , 0xE2
          , 0x80
          , 0x9C
          , 0x71
          , 0x75
          , 0x6F
          , 0x74
          , 0x65
          , 0xE2
          , 0x80
          , 0x9D
          , 0x20
          , 0xE2
          , 0x82
          , 0xAC
          , 0x0A
          ]
          :: BS.ByteString
  (inTmp, inH0) <- openTempFile tmpDir "t4b-in"
  hClose inH0
  BS.writeFile inTmp want
  outRes <- try $ do
    -- Text-mode destination log (the build-log case).
    (textTmp, textH0) <- openTempFile tmpDir "t4b-log-text"
    hClose textH0
    logH <- openFile textTmp WriteMode
    (_, _, _, ph) <-
      Process.createProcess
        (Process.proc "cat" [inTmp])
          { Process.std_out = Process.UseHandle logH
          , Process.std_err = Process.Inherit
          }
    exit1 <- Process.waitForProcess ph
    Process.cleanupProcess (Nothing, Nothing, Nothing, ph)
    hClose logH
    gotText <- BS.readFile textTmp
    -- Binary-mode destination log.
    (binTmp, binH0) <- openTempFile tmpDir "t4b-log-bin"
    hClose binH0
    logB <- openBinaryFile binTmp WriteMode
    (_, _, _, ph2) <-
      Process.createProcess
        (Process.proc "cat" [inTmp])
          { Process.std_out = Process.UseHandle logB
          , Process.std_err = Process.Inherit
          }
    exit2 <- Process.waitForProcess ph2
    Process.cleanupProcess (Nothing, Nothing, Nothing, ph2)
    hClose logB
    gotBin <- BS.readFile binTmp
    -- Text-mode stdin staging: the child echoes staged input.
    (echoTmp, echoH0) <- openTempFile tmpDir "t4b-echo"
    hClose echoH0
    inH <- openFile inTmp ReadMode
    outH <- openBinaryFile echoTmp WriteMode
    (_, _, _, ph3) <-
      Process.createProcess
        (Process.proc "cat" [])
          { Process.std_in = Process.UseHandle inH
          , Process.std_out = Process.UseHandle outH
          , Process.std_err = Process.Inherit
          }
    hClose inH
    exit3 <- Process.waitForProcess ph3
    Process.cleanupProcess (Nothing, Nothing, Nothing, ph3)
    hClose outH
    gotEcho <- BS.readFile echoTmp
    removeFile textTmp
    removeFile binTmp
    removeFile echoTmp
    return (exit1, gotText, exit2, gotBin, exit3, gotEcho)
  removeFile inTmp
  case outRes of
    Left (e :: SomeException) -> do
      putStrLn ("info T4b raised: " ++ show e)
      check "T4b UTF-8 file handles (no exception)" False
    Right (exit1, gotText, exit2, gotBin, exit3, gotEcho) ->
      check
        "T4b UTF-8 file handles round-trip"
        ( exit1 == ExitSuccess
            && gotText == want
            && exit2 == ExitSuccess
            && gotBin == want
            && exit3 == ExitSuccess
            && gotEcho == want
        )

-- T5: exit codes and stream separation.
tExitStreams :: IO (Maybe Failure)
tExitStreams = do
  (exit, out, err) <-
    Process.readCreateProcessWithExitCode
      (Process.proc "sh" ["-c", "echo out; echo err >&2; exit 3"])
      ""
  check "T5 exit code and streams" (exit == ExitFailure 3 && out == "out\n" && err == "err\n")

-- T6: public CreatePipe is rejected under MHS, fine under GHC.
tCreatePipe :: IO (Maybe Failure)
#ifdef __MHS__
tCreatePipe = do
  res <-
    try
      ( Process.createProcess
          (Process.proc "true" [])
            { Process.std_out = Process.CreatePipe
            }
          >> return ()
        )
      :: IO (Either SomeException ())
  case res of
    Left (_ :: SomeException) -> check "T6 CreatePipe explicitly rejected" True
    Right _ -> check "T6 CreatePipe explicitly rejected" False
#else
tCreatePipe = do
  res <-
    try
      ( do
          (_, Just hOut, _, ph) <-
            Process.createProcess
              (Process.proc "echo" ["hi"])
                { Process.std_out = Process.CreatePipe
                }
          out <- hGetContents hOut
          _ <- evaluate (length out)
          exit <- Process.waitForProcess ph
          Process.cleanupProcess (Nothing, Just hOut, Nothing, ph)
          return (exit, out)
      )
      :: IO (Either SomeException (ExitCode, String))
  case res of
    Right (ExitSuccess, "hi\n") -> check "T6 CreatePipe works (GHC)" True
    _ -> check "T6 CreatePipe works (GHC)" False
#endif

-- T7: a failing output handle must surface, never silently succeed.
tFailingHandle :: IO (Maybe Failure)
tFailingHandle = do
  tmpDir <- getTemporaryDirectory
  (tmp, h) <- openTempFile tmpDir "t7-closed"
  hClose h
  res <-
    try
      ( do
          (_, _, _, ph) <-
            Process.createProcess
              (Process.proc "echo" ["hi"])
                { Process.std_out = Process.UseHandle h
                }
          exit <- Process.waitForProcess ph
          Process.cleanupProcess (Nothing, Nothing, Nothing, ph)
          return exit
      )
      :: IO (Either SomeException ExitCode)
  removeFile tmp
  case res of
    Left (_ :: SomeException) -> check "T7 failing handle raises" True
    Right _ -> check "T7 failing handle raises" False

main :: IO ()
main = do
  mark ("backend=" ++ backend)
  results <-
    sequence
      [ tDelayedWriter
      , tNonAscii
      , tEnvRemoval
      , tAcquireRollback
      , tEmptyEnvLookup
      , tMalformedEnv
      , tFileHandles
      , tUtf8FileHandles
      , tExitStreams
      , tCreatePipe
      , tFailingHandle
      ]
  let failures = catMaybes results
  unless (null failures) $ do
    mark ("failures: " ++ show failures)
    exitFailure
  mark "all green"
  exitSuccess
