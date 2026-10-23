module UnitTests.Distribution.Client.Store (tests) where

-- import Control.Monad
-- import Control.Concurrent (forkIO, threadDelay)
-- import Control.Concurrent.MVar

import Data.List (isInfixOf)
import qualified Data.Set as Set
import System.Directory
import System.FilePath

-- import System.Random

import Distribution.Package (UnitId, mkUnitId)
import Distribution.Simple.Compiler (AbiTag (..), Compiler (..), CompilerFlavor (..), CompilerId (..))
import Distribution.Simple.Utils (withTempDirectory)
import Distribution.System (Arch (..), OS (..), Platform (..))
import Distribution.Verbosity
import Distribution.Version (mkVersion)

import Distribution.Client.DistDirLayout (betterPlatform, defaultStoreDirLayout)
import Distribution.Client.ProjectPlanOutput (toolchainJsonFields)
import Distribution.Client.RebuildMonad
import Distribution.Client.Store
import Distribution.Client.Toolchain (Toolchain (..))
import qualified Distribution.Client.Utils.Json as Json
import Distribution.Solver.Types.Stage (Stage (..))
import Distribution.Simple.Program.Db (emptyProgramDb)
import qualified Data.Map as Map

import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ testCase "list content empty" testListEmpty
  , testCase "install serial" testInstallSerial
  , testCase "same architecture target ABIs have separate stores and databases" testTargetAbi
  -- , testCase "install parallel"    testInstallParallel
  -- TODO: figure out some way to do a parallel test, see issue below
  ]

testTargetAbi :: Assertion
testTargetAbi = do
  let compiler =
        Compiler
          { compilerId = CompilerId GHC (mkVersion [9, 14, 1])
          , compilerAbiTag = NoAbiTag
          , compilerCompat = []
          , compilerLanguages = []
          , compilerExtensions = []
          , compilerProperties = mempty
          , compilerWiredInUnitIds = Nothing
          }
      -- A profiling target is identified by its ABI tag, which the store
      -- layout uses to keep its artifacts separate.
      profiledCompiler = compiler{compilerAbiTag = AbiTag "profiling"}
      platform = Platform X86_64 Linux
      layout = defaultStoreDirLayout "store"
  storeDirectory layout compiler platform @?= "store" </> "x86_64-linux" </> "ghc-9.14.1"
  storeDirectory layout profiledCompiler platform @?= "store" </> "x86_64-linux" </> "ghc-9.14.1-profiling"
  assertBool "package databases must not alias" $
    storePackageDBPath layout compiler platform /= storePackageDBPath layout profiledCompiler platform
  -- A qualified target identity renders canonically for plan output and
  -- path helpers, independently of the host architecture.
  let nativeToolchain = Toolchain compiler platform emptyProgramDb
      profiledToolchain =
        Toolchain
          compiler
            { compilerProperties =
                Map.fromList
                  [ ("Target name", "aarch64-apple-darwin-prof")
                  , ("Target platform", "aarch64-apple-darwin")
                  , ("Target ABI", "profiling")
                  ]
            }
          platform
          emptyProgramDb
  betterPlatform nativeToolchain @?= "x86_64-linux"
  betterPlatform profiledToolchain @?= "aarch64-apple-darwin-prof"
  assertBool "incoming locks must use the selected target namespace" $
    storeIncomingLock layout compiler platform (mkUnitId "foo-1.0-xyz")
      /= storeIncomingLock layout profiledCompiler platform (mkUnitId "foo-1.0-xyz")
  case lookup "target-name" (toolchainJsonFields Host profiledToolchain) of
    Just (Json.String targetName) -> targetName @?= "aarch64-apple-darwin-prof"
    _ -> assertFailure "plan JSON omitted the canonical target-name field"
  case lookup "build-target-name" (toolchainJsonFields Build profiledToolchain) of
    Just (Json.String targetName) -> targetName @?= "aarch64-apple-darwin-prof"
    _ -> assertFailure "build-stage plan JSON omitted the canonical target-name field"
  assertBool "the ABI crossing is independent of the architecture" $
    betterPlatform nativeToolchain /= betterPlatform profiledToolchain

testListEmpty :: Assertion
testListEmpty =
  withTempDirectory "." "store-" $ \tmp -> do
    let storeDirLayout = defaultStoreDirLayout (tmp </> "store")

    assertStoreEntryExists storeDirLayout compiler platform unitid False
    assertStoreContent tmp storeDirLayout compiler platform Set.empty
  where
    platform :: Platform
    platform = Platform X86_64 Linux

    compiler :: Compiler
    compiler =
      Compiler
        { compilerId = CompilerId GHC (mkVersion [1, 0])
        , compilerAbiTag = NoAbiTag
        , compilerCompat = []
        , compilerLanguages = []
        , compilerExtensions = []
        , compilerProperties = mempty
        , compilerWiredInUnitIds = Nothing
        }

    unitid = mkUnitId "foo-1.0-xyz"

testInstallSerial :: Assertion
testInstallSerial =
  withTempDirectory "." "store-" $ \tmp -> do
    let storeDirLayout = defaultStoreDirLayout (tmp </> "store")
        copyFiles file content dir = do
          -- we copy into a prefix inside the tmp dir and return the prefix
          let destprefix = dir </> "prefix"
          createDirectory destprefix
          writeFile (destprefix </> file) content
          return (destprefix, [])

    -- newStoreEntry places temporary files inside the store's incoming directory.
    createDirectoryIfMissing True (storeIncomingDirectory storeDirLayout compiler platform)

    assertNewStoreEntry
      tmp
      storeDirLayout
      compiler
      platform
      unitid1
      (copyFiles "file1" "content-foo")
      (return ())
      UseNewStoreEntry

    assertNewStoreEntry
      tmp
      storeDirLayout
      compiler
      platform
      unitid1
      (copyFiles "file1" "content-foo")
      (return ())
      UseExistingStoreEntry

    assertNewStoreEntry
      tmp
      storeDirLayout
      compiler
      platform
      unitid2
      (copyFiles "file2" "content-bar")
      (return ())
      UseNewStoreEntry

    let pkgDir :: UnitId -> FilePath
        pkgDir = storePackageDirectory storeDirLayout compiler platform
    assertFileEqual (pkgDir unitid1 </> "file1") "content-foo"
    assertFileEqual (pkgDir unitid2 </> "file2") "content-bar"
  where
    platform :: Platform
    platform = Platform X86_64 Linux

    compiler :: Compiler
    compiler =
      Compiler
        { compilerId = CompilerId GHC (mkVersion [1, 0])
        , compilerAbiTag = NoAbiTag
        , compilerCompat = []
        , compilerLanguages = []
        , compilerExtensions = []
        , compilerProperties = mempty
        , compilerWiredInUnitIds = Nothing
        }

    unitid1 = mkUnitId "foo-1.0-xyz"
    unitid2 = mkUnitId "bar-2.0-xyz"

{-
-- unfortunately a parallel test like the one below is thwarted by the normal
-- process-internal file locking. If that locking were not in place then we
-- ought to get the blocking behaviour, but due to the normal Handle locking
-- it just fails instead.

testInstallParallel :: Assertion
testInstallParallel =
  withTempDirectory "." "store-" $ \tmp -> do
    let storeDirLayout = defaultStoreDirLayout (tmp </> "store")

    sync1 <- newEmptyMVar
    sync2 <- newEmptyMVar
    outv  <- newEmptyMVar
    regv  <- newMVar (0 :: Int)

    sequence_
      [ do forkIO $ do
             let copyFiles dir = do
                   delay <- randomRIO (1,100000)
                   writeFile (dir </> "file") (show n)
                   putMVar  sync1 ()
                   readMVar sync2
                   threadDelay delay
                 register = do
                   modifyMVar_ regv (return . (+1))
                   threadDelay 200000
             o <- newStoreEntry verbosity storeDirLayout
                                compid unitid
                                copyFiles register
             putMVar outv (n, o)
      | n <- [0..9 :: Int] ]

    replicateM_ 10 (takeMVar sync1)
    -- all threads are in the copyFiles action concurrently, release them:
    putMVar  sync2 ()

    outcomes <- replicateM 10 (takeMVar outv)
    regcount <- readMVar regv
    let regcount' = length [ () | (_, UseNewStoreEntry) <- outcomes ]

    assertEqual "num registrations" 1 regcount
    assertEqual "num registrations" 1 regcount'

    assertStoreContent tmp storeDirLayout compid (Set.singleton unitid)

    let pkgDir :: UnitId -> FilePath
        pkgDir = storePackageDirectory storeDirLayout compid
    case [ n | (n, UseNewStoreEntry) <- outcomes ] of
      [n] -> assertFileEqual (pkgDir unitid </> "file") (show n)
      _   -> assertFailure "impossible"

  where
    compid  = CompilerId GHC (mkVersion [1,0])
    unitid = mkUnitId "foo-1.0-xyz"
-}

-------------
-- Utils

assertNewStoreEntry
  :: FilePath
  -> StoreDirLayout
  -> Compiler
  -> Platform
  -> UnitId
  -> (FilePath -> IO (FilePath, [FilePath]))
  -> IO ()
  -> NewStoreEntryOutcome
  -> Assertion
assertNewStoreEntry
  tmp
  storeDirLayout
  compiler
  platform
  unitid
  copyFiles
  register
  expectedOutcome = do
    entries <- runRebuild tmp $ getStoreEntries storeDirLayout compiler platform
    outcome <-
      newStoreEntry
        verbosity
        storeDirLayout
        compiler
        platform
        unitid
        copyFiles
        register
    assertEqual "newStoreEntry outcome" expectedOutcome outcome
    assertStoreEntryExists storeDirLayout compiler platform unitid True
    let expected = Set.insert unitid entries
    assertStoreContent tmp storeDirLayout compiler platform expected

assertStoreEntryExists
  :: StoreDirLayout
  -> Compiler
  -> Platform
  -> UnitId
  -> Bool
  -> Assertion
assertStoreEntryExists storeDirLayout compiler platform unitid expected = do
  actual <- doesStoreEntryExist storeDirLayout compiler platform unitid
  assertEqual "store entry exists" expected actual

assertStoreContent
  :: FilePath
  -> StoreDirLayout
  -> Compiler
  -> Platform
  -> Set.Set UnitId
  -> Assertion
assertStoreContent tmp storeDirLayout compiler platform expected = do
  actual <- runRebuild tmp $ getStoreEntries storeDirLayout compiler platform
  assertEqual "store content" actual expected

assertFileEqual :: FilePath -> String -> Assertion
assertFileEqual path expected = do
  exists <- doesFileExist path
  assertBool ("file does not exist:\n" ++ path) exists
  actual <- readFile path
  assertEqual ("file content for:\n" ++ path) expected actual

verbosity :: Verbosity
verbosity = mkVerbosity defaultVerbosityHandles silent
