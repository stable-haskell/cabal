module UnitTests.Distribution.Client.Store (tests) where

-- import Control.Monad
-- import Control.Concurrent (forkIO, threadDelay)
-- import Control.Concurrent.MVar

import qualified Data.Set as Set
import System.Directory
import System.FilePath

-- import System.Random

import Distribution.Package (UnitId, mkUnitId)
import Distribution.Simple.Compiler (AbiTag (..), Compiler (..), CompilerFlavor (..), CompilerId (..))
import Distribution.Simple.Program.Db (emptyProgramDb)
import Distribution.Simple.Utils (withTempDirectory)
import Distribution.System (Arch (..), OS (..), Platform (..), buildPlatform)
import Distribution.Verbosity
import Distribution.Version (mkVersion)

import qualified Data.Map as Map
import Distribution.Client.DistDirLayout (betterPlatform)
import Distribution.Client.ProjectPlanOutput (toolchainJsonFields)
import Distribution.Client.RebuildMonad
import Distribution.Client.Store
import Distribution.Client.Toolchain (Machine (..), Toolchain (..))
import qualified Distribution.Client.Utils.Json as Json

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
      platform = Platform X86_64 Linux
      layout = defaultStoreDirLayout "store"
      -- The plain toolchain reports no target properties, so the store
      -- uses cabal's own platform spelling.
      nativeToolchain = Toolchain compiler platform emptyProgramDb
      -- A same-architecture profiling target keeps its store directory,
      -- package database and incoming lock disjoint via the -prof suffix.
      profiledToolchain =
        Toolchain
          compiler
            { compilerProperties =
                Map.fromList
                  [ ("Target platform", "x86_64-linux")
                  , ("Target ABI", "profiling")
                  ]
            }
          platform
          emptyProgramDb
      -- A qualified target identity renders canonically for plan output
      -- and path helpers, independently of the host architecture.
      crossToolchain =
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
  storeDirectory layout Host nativeToolchain @?= "store" </> "host" </> "x86_64-linux"
  storeDirectory layout Host profiledToolchain @?= "store" </> "host" </> "x86_64-linux-prof"
  assertBool "package databases must not alias" $
    storePackageDBPath layout Host nativeToolchain /= storePackageDBPath layout Host profiledToolchain
  assertBool "incoming locks must use the selected target namespace" $
    storeIncomingLock layout Host nativeToolchain (mkUnitId "foo-1.0-xyz")
      /= storeIncomingLock layout Host profiledToolchain (mkUnitId "foo-1.0-xyz")
  betterPlatform nativeToolchain @?= "x86_64-linux"
  betterPlatform crossToolchain @?= "aarch64-apple-darwin-prof"
  case lookup "target-name" (toolchainJsonFields Host crossToolchain) of
    Just (Json.String targetName) -> targetName @?= "aarch64-apple-darwin-prof"
    _ -> assertFailure "plan JSON omitted the canonical target-name field"
  case lookup "build-target-name" (toolchainJsonFields Build crossToolchain) of
    Just (Json.String targetName) -> targetName @?= "aarch64-apple-darwin-prof"
    _ -> assertFailure "build-stage plan JSON omitted the canonical target-name field"
  assertBool "the ABI crossing is independent of the architecture" $
    betterPlatform nativeToolchain /= betterPlatform profiledToolchain

testListEmpty :: Assertion
testListEmpty =
  withTempDirectory "." "store-" $ \tmp -> do
    let storeDirLayout = defaultStoreDirLayout (tmp </> "store")

    assertStoreEntryExists storeDirLayout toolchain unitid False
    assertStoreContent tmp storeDirLayout toolchain Set.empty
  where
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

    assertNewStoreEntry
      tmp
      storeDirLayout
      toolchain
      unitid1
      (copyFiles "file1" "content-foo")
      (return ())
      UseNewStoreEntry

    assertNewStoreEntry
      tmp
      storeDirLayout
      toolchain
      unitid1
      (copyFiles "file1" "content-foo")
      (return ())
      UseExistingStoreEntry

    assertNewStoreEntry
      tmp
      storeDirLayout
      toolchain
      unitid2
      (copyFiles "file2" "content-bar")
      (return ())
      UseNewStoreEntry

    let pkgDir :: UnitId -> FilePath
        pkgDir = storePackageDirectory storeDirLayout Host toolchain
    assertFileEqual (pkgDir unitid1 </> "file1") "content-foo"
    assertFileEqual (pkgDir unitid2 </> "file2") "content-bar"

    -- Store reuse is disabled under this layout (see Note [Store reuse is
    -- disabled under this layout] in Distribution.Client.Store): installed
    -- units live under the store's shared lib/ and are never enumerated by
    -- getStoreEntries.
    entries <- runRebuild tmp $ getStoreEntries storeDirLayout Host toolchain
    assertBool "installed unit must not be enumerated while reuse is disabled" $
      Set.notMember unitid1 entries
    assertBool "installed unit must not be enumerated while reuse is disabled" $
      Set.notMember unitid2 entries
  where
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

-- | A dummy toolchain: the store layout only reads the compiler's @Target
-- platform@ property (absent here, so 'betterPlatform' falls back to the
-- toolchain's platform).
toolchain :: Toolchain
toolchain =
  Toolchain
    { toolchainCompiler =
        Compiler
          { compilerId = CompilerId GHC (mkVersion [1, 0])
          , compilerAbiTag = NoAbiTag
          , compilerCompat = []
          , compilerLanguages = []
          , compilerExtensions = []
          , compilerProperties = mempty
          , compilerWiredInUnitIds = Nothing
          }
    , toolchainPlatform = buildPlatform
    , toolchainProgramDb = emptyProgramDb
    }

assertNewStoreEntry
  :: FilePath
  -> StoreDirLayout
  -> Toolchain
  -> UnitId
  -> (FilePath -> IO (FilePath, [FilePath]))
  -> IO ()
  -> NewStoreEntryOutcome
  -> Assertion
assertNewStoreEntry
  _tmp
  storeDirLayout
  tc
  unitid
  copyFiles
  register
  expectedOutcome = do
    outcome <-
      newStoreEntry
        verbosity
        storeDirLayout
        Host
        tc
        unitid
        copyFiles
        register
    assertEqual "newStoreEntry outcome" expectedOutcome outcome
    assertStoreEntryExists storeDirLayout tc unitid True

assertStoreEntryExists
  :: StoreDirLayout
  -> Toolchain
  -> UnitId
  -> Bool
  -> Assertion
assertStoreEntryExists storeDirLayout tc unitid expected = do
  actual <- doesStoreEntryExist storeDirLayout Host tc unitid
  assertEqual "store entry exists" expected actual

assertStoreContent
  :: FilePath
  -> StoreDirLayout
  -> Toolchain
  -> Set.Set UnitId
  -> Assertion
assertStoreContent tmp storeDirLayout tc expected = do
  actual <- runRebuild tmp $ getStoreEntries storeDirLayout Host tc
  assertEqual "store content" actual expected

assertFileEqual :: FilePath -> String -> Assertion
assertFileEqual path expected = do
  exists <- doesFileExist path
  assertBool ("file does not exist:\n" ++ path) exists
  actual <- readFile path
  assertEqual ("file content for:\n" ++ path) expected actual

verbosity :: Verbosity
verbosity = mkVerbosity defaultVerbosityHandles silent
