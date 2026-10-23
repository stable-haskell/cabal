module UnitTests.Distribution.Client.InLibrary (tests) where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Distribution.Client.InLibrary as InLibrary
import Distribution.Client.Toolchain (Toolchain (..), getStage)
import Distribution.Client.Types (GenericReadyPackage (..))
import Distribution.Simple.Compiler
import Distribution.Simple.Program.Builtin (ghcProgram)
import Distribution.Simple.Program.Db
import Distribution.Simple.Program.Types
import Distribution.Solver.Types.Stage (Stage (..), Staged (..))
import Distribution.System (Arch (AArch64, X86_64), OS (Linux, OSX), Platform (..))
import Distribution.Types.BuildType (BuildType (Simple))
import qualified Distribution.Types.LocalBuildConfig as LBC
import Distribution.Verbosity (defaultVerbosityHandles)
import Distribution.Version (mkVersion)
import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ testCase "in-library Build configuration pairs native metadata and program" $ checkToolchain Build
  , testCase "in-library Host configuration pairs profiling metadata and program" $ checkToolchain Host
  ]

checkToolchain :: Stage -> Assertion
checkToolchain stage = do
  let toolchain = getStage toolchains stage
      compilerProgram = fromMaybe (error "missing compiler program") $ lookupProgram ghcProgram (toolchainProgramDb toolchain)
      packageProgram = compilerProgram{programOverrideArgs = ["-package-specific-option"]}
      packagePrograms = updateProgram packageProgram (toolchainProgramDb toolchain)
      inputs =
        InLibrary.libraryConfigureInputsFromElabPackage
          defaultVerbosityHandles
          Simple
          packagePrograms
          toolchain
          -- Compiler metadata must not depend on the package description.
          (ReadyPackage (error "unexpected package metadata evaluation"))
          mempty
          []
      configuredProgram = lookupProgram ghcProgram (LBC.withPrograms (InLibrary.localBuildConfig inputs))
  InLibrary.compiler inputs @?= toolchainCompiler toolchain
  InLibrary.platform inputs @?= toolchainPlatform toolchain
  fmap programPath configuredProgram @?= Just (programPath packageProgram)
  fmap programOverrideArgs configuredProgram @?= Just ["-package-specific-option"]
  fmap (Map.findWithDefault "" "stage-marker" . programProperties) configuredProgram
    @?= Just (Map.findWithDefault "" "stage-marker" (compilerProperties (InLibrary.compiler inputs)))

toolchains :: Staged Toolchain
toolchains =
  Staged
    { onHost = makeToolchain "host" "host-ghc" (Platform AArch64 OSX)
    , onBuild = Just (makeToolchain "build" "native-ghc" (Platform X86_64 Linux))
    }

makeToolchain :: String -> FilePath -> Platform -> Toolchain
makeToolchain stageMarker path platform =
  Toolchain
    Compiler
      { compilerId = CompilerId GHC (if stageMarker == "build" then mkVersion [9, 12] else mkVersion [9, 14])
      , compilerAbiTag = AbiTag (stageMarker ++ "-abi")
      , compilerCompat = []
      , compilerLanguages = []
      , compilerExtensions = []
      , compilerProperties = Map.singleton "stage-marker" stageMarker
      , compilerWiredInUnitIds = Nothing
      }
    platform
    ( updateProgram
        ( (simpleConfiguredProgram "ghc" (UserSpecified path))
            { programProperties = Map.singleton "stage-marker" stageMarker
            }
        )
        emptyProgramDb
    )
