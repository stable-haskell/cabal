module UnitTests.Distribution.Simple.GHC (tests) where

import Data.List (isInfixOf)
import Data.Either (isLeft)
import Control.Exception (SomeException, displayException, try)
import qualified Data.Map as Map
import Distribution.Simple.BuildWay (BuildWay (..), buildWayInterfaceExtension, buildWayObjectExtension)
import Distribution.Simple.Compiler
import Distribution.Simple.Configure (adjustBuildOptions, configureProfiling, profilingTargetError, profilingTargetVariantError)
import Distribution.Simple.Errors (CabalException (ProfilingRequiresTargetCompiler), exceptionMessage)
import Distribution.Simple.Flag
import Distribution.Simple.Setup (ConfigFlags (..), emptyConfigFlags)
import qualified Distribution.Simple.GHC as GHC
import Distribution.Simple.Program.Db (emptyProgramDb)
import Distribution.System (Arch (X86_64), OS (Linux), Platform (..))
import qualified Distribution.Types.LocalBuildConfig as LBC
import Distribution.Version (mkVersion)
import Distribution.Verbosity (defaultVerbosityHandles, mkVerbosity, silent)
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
            "--with-compiler=<machine>-prof-ghc" `isInfixOf` exceptionMessage err
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
  , testCase "unavailable profiling shared output reports target and alternatives" $ do
      let target =
            withProperty "Target name" "x86_64-unknown-linux-prof" $
              withProperty "RTS ways" "v p thr" profilingCompiler
      case profilingTargetVariantError target "shared profiling libraries" (profilingDynamicSupported target) of
        Nothing -> assertFailure "unsupported shared profiling output was accepted"
        Just err -> do
          assertBool "diagnostic omits selected target" $ "x86_64-unknown-linux-prof" `isInfixOf` exceptionMessage err
          assertBool "diagnostic omits requested variant" $ "shared profiling libraries" `isInfixOf` exceptionMessage err
          assertBool "diagnostic omits static alternative" $ "static nondebug" `isInfixOf` exceptionMessage err
  , testCase "Setup configure rejects an explicit unavailable shared request" $ do
      let target =
            withProperty "RTS ways" "v thr" $
              withProperty "Target name" "x86_64-unknown-linux-prof" $
                withProperty "Target platform" "x86_64-unknown-linux" profilingCompiler
          cfg = emptyConfigFlags{configSharedLib = Flag True}
      result <- try (configureProfiling (mkVerbosity defaultVerbosityHandles silent) cfg target) :: IO (Either SomeException (LBC.BuildOptions -> LBC.BuildOptions))
      case result of
        Left err -> do
          assertBool "configure error omits target" $ "x86_64-unknown-linux-prof" `isInfixOf` displayException err
          assertBool "configure error omits unsupported variant" $ "shared libraries" `isInfixOf` displayException err
        Right _ -> assertFailure "configure accepted an unavailable shared variant"
  , testCase "unknown profiling shared capability remains permissive" $
      case profilingTargetVariantError (profilingCompiler{compilerId = CompilerId GHC (mkVersion [9, 8, 4])}) "shared libraries" Nothing of
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
  , testCase "profiling debug capability uses target's ordinary RTS ways" $ do
      profilingDebugSupported (withProperty "RTS ways" "v thr" profilingCompiler) @?= Just False
      profilingDebugSupported (withProperty "RTS ways" "v debug thr_debug" profilingCompiler) @?= Just True
      profilingDebugSupported ordinaryCompiler @?= Just False
  , testCase "user DB and environment ABI suffix follows compiler version" $ do
      let platform = Platform X86_64 Linux
          version = mkVersion [9, 14, 1]
      GHC.ghcPlatformAndVersionStringWithAbi platform version "profiling"
        @?= "x86_64-linux-9.14.1-profiling"
      GHC.ghcPlatformAndVersionStringWithAbi platform version ""
        @?= GHC.ghcPlatformAndVersionString platform version
  , testCase "canonical target names retain the machine environment" $ do
      let platform = Platform X86_64 Linux
          comp = withProperty "Target ABI" "profiling" $
            withProperty "Target platform" "arm-unknown-linux-gnueabihf" $
              withProperty "Target name" "arm-unknown-linux-gnueabihf-prof" ordinaryCompiler
      actual <- either assertFailure (pure . renderTargetName) (compilerTargetName platform comp)
      actual @?= "arm-unknown-linux-gnueabihf-prof"
  , testCase "new target metadata rejects contradictory ABI names" $ do
      let platform = Platform X86_64 Linux
          comp = withProperty "Target ABI" "" $
            withProperty "Target platform" "arm-unknown-linux-gnueabihf" $
              withProperty "Target name" "arm-unknown-linux-gnueabihf-prof" ordinaryCompiler
      case compilerTargetName platform comp of
        Left _ -> pure ()
        Right name -> assertFailure ("accepted contradictory target " ++ renderTargetName name)
  , testCase "qualified target plus compiler version fits filesystem component" $ do
      let longMachine = replicate 250 'a'
          platform = Platform X86_64 Linux
          comp =
            withProperty "GHC Profiled" "NO" $
              withProperty "Target platform" longMachine $
                withProperty "Target name" longMachine ordinaryCompiler
      case compilerTargetName platform comp of
        Left problem -> assertBool "combined compiler version limit was not reported" $
          "compiler version exceeds" `isInfixOf` problem
        Right name -> assertFailure ("accepted overlong path component " ++ renderTargetName name)
  , testCase "target names reject path and whitespace characters" $ do
      mapM_ (assertBool "unsafe target name accepted" . isLeft . parseTargetName) ["", "a/b", "a b", "a--b", "../x", replicate 256 'a']
  , testCase "legacy target metadata keeps its platform ABI fallback" $ do
      let platform = Platform X86_64 Linux
          comp = withProperty "Target ABI" "profiling" $
            withProperty "Target platform" "arm-unknown-linux-gnueabihf" ordinaryCompiler
      actual <- either assertFailure (pure . renderTargetName) (compilerTargetName platform comp)
      actual @?= "arm-unknown-linux-gnueabihf-prof"
  , testCase "new namespace uses canonical target and version" $ do
      GHC.ghcPlatformAndVersionStringWithTarget (Platform X86_64 Linux) (mkVersion [9, 14, 1]) (Just "arm-unknown-linux-gnueabihf-prof") "profiling"
        @?= "arm-unknown-linux-gnueabihf-prof-9.14.1"
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
