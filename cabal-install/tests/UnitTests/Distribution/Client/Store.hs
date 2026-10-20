module UnitTests.Distribution.Client.Store (tests) where

-- import Control.Monad
-- import Control.Concurrent (forkIO, threadDelay)
-- import Control.Concurrent.MVar

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (isInfixOf, isSuffixOf, nub)
import System.Directory
import System.FilePath

-- import System.Random

import Distribution.Package (UnitId, mkUnitId)
import Distribution.Simple.Compiler (AbiTag (..), Compiler (..), CompilerFlavor (..), CompilerId (..))
import Distribution.Simple.Program.Db (emptyProgramDb)
import Distribution.Simple.Utils (withTempDirectory)
import Distribution.System (buildPlatform)
import Distribution.Verbosity
import Distribution.Version (mkVersion)

import Distribution.Client.RebuildMonad
import Distribution.Client.DistDirLayout (DistDirParams (..), ProjectRoot (..), defaultDistDirLayout, distBuildDirectory)
import Distribution.Client.ProjectPlanOutput (toolchainJsonFields)
import Distribution.Client.Store
import qualified Distribution.Client.Utils.Json as Json
import Distribution.Solver.Types.Stage (Stage (..))
import Distribution.Solver.Types.Toolchain (Toolchain (..))

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
          , compilerProperties =
              Map.fromList
                [ ("Target name", "aarch64-apple-darwin")
                , ("Target platform", "aarch64-apple-darwin")
                ]
          , compilerWiredInUnitIds = Nothing
          }
      native = Toolchain buildPlatform compiler emptyProgramDb []
      profiled =
        native
          { toolchainCompiler =
              compiler
                { compilerProperties =
                    Map.insert "Target name" "aarch64-apple-darwin-prof" $
                      Map.insert "Target ABI" "profiling" (compilerProperties compiler)
                }
          }
      layout = defaultStoreDirLayout "store"
  storeDirectory layout Host native @?= "store" </> "host" </> "aarch64-apple-darwin"
  storeDirectory layout Host profiled @?= "store" </> "host" </> "aarch64-apple-darwin-prof"
  assertBool "package databases must not alias" $
    storePackageDBPath layout Host native /= storePackageDBPath layout Host profiled
  assertBool "incoming locks must use the selected target namespace" $
    storeIncomingLock layout Host native (mkUnitId "foo-1.0-xyz")
      /= storeIncomingLock layout Host profiled (mkUnitId "foo-1.0-xyz")
  let distLayout = defaultDistDirLayout (ProjectRootImplicit "project") Nothing Nothing
      nativeDist = distBuildDirectory distLayout (DistDirParams Host native (mkUnitId "foo-1.0-xyz"))
      profiledDist = distBuildDirectory distLayout (DistDirParams Host profiled (mkUnitId "foo-1.0-xyz"))
  assertBool "dist build paths must use the canonical target namespace" (nativeDist /= profiledDist)
  assertBool "dist path must contain canonical prof target" ("aarch64-apple-darwin-prof" `isInfixOf` profiledDist)
  case lookup "target-name" (toolchainJsonFields Host profiled) of
    Just (Json.String targetName) -> targetName @?= "aarch64-apple-darwin-prof"
    _ -> assertFailure "plan JSON omitted the canonical target-name field"
  case lookup "build-target-name" (toolchainJsonFields Build profiled) of
    Just (Json.String targetName) -> targetName @?= "aarch64-apple-darwin-prof"
    _ -> assertFailure "build-stage plan JSON omitted the canonical target-name field"
  assertBool "the ABI crossing is independent of the architecture" (native /= profiled)
  let armMachines = ["arm-unknown-linux-gnueabi", "arm-unknown-linux-gnueabihf"]
      armTargets = armMachines ++ map (++ "-prof") armMachines
      armToolchains = map armToolchain armTargets
      armStores = map (storeDirectory layout Host) armToolchains
      armDatabases = map (storePackageDBPath layout Host) armToolchains
      armLocks = map (\tc -> storeIncomingLock layout Host tc (mkUnitId "foo-1.0-xyz")) armToolchains
      armDists = map (\tc -> distBuildDirectory distLayout (DistDirParams Host tc (mkUnitId "foo-1.0-xyz"))) armToolchains
      armJsonTargets = map targetNameFromJson armToolchains
      armBuildJsonTargets = map buildTargetNameFromJson armToolchains
      armToolchain targetName =
        let profiling = "-prof" `isSuffixOf` targetName
            machine = if profiling then take (length targetName - length "-prof") targetName else targetName
            targetCompiler =
              compiler
                { compilerProperties =
                    Map.fromList
                      [ ("Target name", targetName)
                      , ("Target platform", machine)
                      , ("Target ABI", if profiling then "profiling" else "")
                      ]
                }
         in native{toolchainCompiler = targetCompiler}
      targetNameFromJson toolchain =
        case lookup "target-name" (toolchainJsonFields Host toolchain) of
          Just (Json.String targetName) -> Just targetName
          _ -> Nothing
      buildTargetNameFromJson toolchain =
        case lookup "build-target-name" (toolchainJsonFields Build toolchain) of
          Just (Json.String targetName) -> Just targetName
          _ -> Nothing
  length (nub armStores) @?= 4
  length (nub armDatabases) @?= 4
  length (nub armLocks) @?= 4
  length (nub armDists) @?= 4
  armJsonTargets @?= map Just armTargets
  armBuildJsonTargets @?= map Just armTargets

testListEmpty :: Assertion
testListEmpty =
  withTempDirectory "." "store-" $ \tmp -> do
    let storeDirLayout = defaultStoreDirLayout (tmp </> "store")

    assertStoreEntryExists storeDirLayout stage toolchain unitid False
    assertStoreContent tmp storeDirLayout stage toolchain Set.empty
  where
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
    toolchain = Toolchain buildPlatform compiler emptyProgramDb []
    stage = Host

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

    -- newStoreEntry places temporary files inside the stage's store.
    createDirectoryIfMissing True (storeIncomingDirectory storeDirLayout stage toolchain)

    assertNewStoreEntry
      tmp
      storeDirLayout
      stage
      toolchain
      unitid1
      (copyFiles "file1" "content-foo")
      (return ())
      UseNewStoreEntry

    assertNewStoreEntry
      tmp
      storeDirLayout
      stage
      toolchain
      unitid1
      (copyFiles "file1" "content-foo")
      (return ())
      UseExistingStoreEntry

    assertNewStoreEntry
      tmp
      storeDirLayout
      stage
      toolchain
      unitid2
      (copyFiles "file2" "content-bar")
      (return ())
      UseNewStoreEntry

    let pkgDir :: UnitId -> FilePath
        pkgDir = storePackageDirectory storeDirLayout stage toolchain
    assertFileEqual (pkgDir unitid1 </> "file1") "content-foo"
    assertFileEqual (pkgDir unitid2 </> "file2") "content-bar"
  where
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
    toolchain = Toolchain buildPlatform compiler emptyProgramDb []
    stage = Host

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
  -> Stage
  -> Toolchain
  -> UnitId
  -> (FilePath -> IO (FilePath, [FilePath]))
  -> IO ()
  -> NewStoreEntryOutcome
  -> Assertion
assertNewStoreEntry
  tmp
  storeDirLayout
  stage
  toolchain
  unitid
  copyFiles
  register
  expectedOutcome = do
    entries <- runRebuild tmp $ getStoreEntries storeDirLayout stage toolchain
    outcome <-
      newStoreEntry
        verbosity
        storeDirLayout
        stage
        toolchain
        unitid
        copyFiles
        register
    assertEqual "newStoreEntry outcome" expectedOutcome outcome
    assertStoreEntryExists storeDirLayout stage toolchain unitid True
    let expected = Set.insert unitid entries
    assertStoreContent tmp storeDirLayout stage toolchain expected

assertStoreEntryExists
  :: StoreDirLayout
  -> Stage
  -> Toolchain
  -> UnitId
  -> Bool
  -> Assertion
assertStoreEntryExists storeDirLayout stage toolchain unitid expected = do
  actual <- doesStoreEntryExist storeDirLayout stage toolchain unitid
  assertEqual "store entry exists" expected actual

assertStoreContent
  :: FilePath
  -> StoreDirLayout
  -> Stage
  -> Toolchain
  -> Set.Set UnitId
  -> Assertion
assertStoreContent tmp storeDirLayout stage toolchain expected = do
  actual <- runRebuild tmp $ getStoreEntries storeDirLayout stage toolchain
  assertEqual "store content" actual expected

assertFileEqual :: FilePath -> String -> Assertion
assertFileEqual path expected = do
  exists <- doesFileExist path
  assertBool ("file does not exist:\n" ++ path) exists
  actual <- readFile path
  assertEqual ("file content for:\n" ++ path) expected actual

verbosity :: Verbosity
verbosity = mkVerbosity defaultVerbosityHandles silent
