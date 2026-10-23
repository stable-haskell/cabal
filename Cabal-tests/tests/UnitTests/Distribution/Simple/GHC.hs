module UnitTests.Distribution.Simple.GHC (tests) where

import Data.List (isInfixOf)
import qualified Data.Map as Map
import Distribution.Simple.BuildWay
import Distribution.Simple.Compiler
import Distribution.Simple.Configure (adjustBuildOptions, profilingTargetError)
import Distribution.Simple.Errors (CabalException (ProfilingRequiresTargetCompiler), exceptionMessage)
import qualified Distribution.Simple.GHC as GHC
import Distribution.Simple.Program.Db (emptyProgramDb)
import Distribution.System (Arch (X86_64), OS (Linux), Platform (..))
import qualified Distribution.Types.LocalBuildConfig as LBC
import Distribution.Version (mkVersion)
import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ testCase "intrinsic profiling uses ordinary library filenames" $ do
      let adjusted = adjustBuildOptions profilingCompiler emptyProgramDb profilingOptions
      LBC.withVanillaLib adjusted @?= True
      LBC.withProfLib adjusted @?= False
      LBC.withProfExe adjusted @?= False
      LBC.withProfLibDetail adjusted @?= ProfDetailAllFunctions
      LBC.withProfExeDetail adjusted @?= ProfDetailToplevelFunctions
  , testCase "intrinsic profiling shared way uses ordinary shared libraries" $ do
      let adjusted =
            adjustBuildOptions
              profilingCompiler
              emptyProgramDb
              profilingOptions
                { LBC.withProfLibShared = True
                , LBC.withSharedLib = False
                }
      LBC.withSharedLib adjusted @?= True
      LBC.withProfLibShared adjusted @?= False
  , testCase "ordinary compiler profiling requests require a target compiler" $ do
      case profilingTargetError ordinaryCompiler profilingOptions of
        Just err@(ProfilingRequiresTargetCompiler _) -> do
          assertBool "migration names the target compiler" $
            "--with-compiler=<platform>-profiling-ghc" `isInfixOf` exceptionMessage err
          assertBool "migration preserves native build tools" $
            "--with-build-compiler=ghc" `isInfixOf` exceptionMessage err
        _ -> assertFailure "ordinary profiling request was accepted"
  , testCase "normal older GHC builds need no profiling target" $ do
      let ordinaryOptions =
            profilingOptions
              { LBC.withProfLib = False
              , LBC.withProfExe = False
              , LBC.withProfLibDetail = ProfDetailDefault
              , LBC.withProfExeDetail = ProfDetailDefault
              }
          older = ordinaryCompiler{compilerId = CompilerId GHC (mkVersion [9, 8, 4])}
      case profilingTargetError older ordinaryOptions of
        Nothing -> pure ()
        Just err -> assertFailure (exceptionMessage err)
  , testCase "profiling instrumentation is accepted only by its target" $ do
      case profilingTargetError profilingCompiler profilingOptions of
        Nothing -> pure ()
        Just err -> assertFailure (exceptionMessage err)
  , testCase "artifact ways have only ordinary static and dynamic names" $ do
      buildWayObjectExtension "o" StaticWay @?= "o"
      buildWayInterfaceExtension StaticWay @?= "hi"
      buildWayObjectExtension "o" DynWay @?= "dyn_o"
      buildWayInterfaceExtension DynWay @?= "dyn_hi"
  , testCase "target profiling is independent of legacy GHC Profiled" $ do
      GHC.compilerBuildWay profilingCompiler @?= StaticWay
      GHC.compilerBuildWay (withProperty "GHC Profiled" "YES" profilingCompiler) @?= StaticWay
      GHC.compilerBuildWay (withProperty "GHC Dynamic" "YES" profilingCompiler) @?= DynWay
      GHC.compilerBuildWay (withProperty "GHC Profiled" "YES" ordinaryCompiler) @?= StaticWay
  , testCase "intrinsic profiling capability needs no legacy RTS p way" $ do
      profilingVanillaSupported profilingCompiler @?= Just True
      profilingVanillaSupported ordinaryCompiler @?= Just False
      profilingVanillaSupported (withProperty "RTS ways" "v p p_dyn" ordinaryCompiler) @?= Just False
      profilingDynamicSupported (withProperty "RTS ways" "v dyn" profilingCompiler) @?= Just True
  , testCase "user DB and environment ABI suffix follows compiler version" $ do
      let platform = Platform X86_64 Linux
          version = mkVersion [9, 14, 1]
      GHC.ghcPlatformAndVersionStringWithAbi platform version "profiling"
        @?= "x86_64-linux-9.14.1-profiling"
      GHC.ghcPlatformAndVersionStringWithAbi platform version ""
        @?= GHC.ghcPlatformAndVersionString platform version
  ]

profilingOptions :: LBC.BuildOptions
profilingOptions =
  LBC.BuildOptions
    { LBC.withVanillaLib = False
    , LBC.withProfLib = True
    , LBC.withProfLibShared = False
    , LBC.withSharedLib = False
    , LBC.withStaticLib = False
    , LBC.withBytecodeLib = False
    , LBC.withDynExe = False
    , LBC.withFullyStaticExe = False
    , LBC.withProfExe = True
    , LBC.withProfLibDetail = ProfDetailAllFunctions
    , LBC.withProfExeDetail = ProfDetailToplevelFunctions
    , LBC.withOptimization = NormalOptimisation
    , LBC.withDebugInfo = NoDebugInfo
    , LBC.withGHCiLib = False
    , LBC.splitSections = False
    , LBC.splitObjs = False
    , LBC.stripExes = False
    , LBC.stripLibs = False
    , LBC.exeCoverage = False
    , LBC.libCoverage = False
    , LBC.relocatable = False
    }

ordinaryCompiler :: Compiler
ordinaryCompiler =
  Compiler
    { compilerId = CompilerId GHC (mkVersion [9, 14, 1])
    , compilerAbiTag = NoAbiTag
    , compilerCompat = []
    , compilerLanguages = []
    , compilerExtensions = []
    , compilerWiredInUnitIds = Nothing
    , compilerProperties = Map.fromList [("GHC Dynamic", "NO"), ("GHC Profiled", "NO")]
    }

profilingCompiler :: Compiler
profilingCompiler = withProperty "Target ABI" "profiling" ordinaryCompiler

withProperty :: String -> String -> Compiler -> Compiler
withProperty key value comp =
  comp
    { compilerProperties = Map.insert key value (compilerProperties comp)
    }
