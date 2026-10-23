{-# LANGUAGE OverloadedStrings #-}

module UnitTests.Distribution.Client.ProjectPlanning (tests) where

import Data.Binary (decode, encode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.List (isInfixOf, nub)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as Map
import Distribution.Backpack (OpenUnitId (..))
import Distribution.Client.CmdInstall (addLocalConfigToPkgs)
import Distribution.Client.HashValue (hashValue)
import Distribution.Client.PackageHash
import Distribution.Client.ProjectConfig (lookupLocalPackageConfig, resolveSolverSettings)
import Distribution.Client.ProjectConfig.Types (MapMappend (..), PackageConfig (..), ProjectConfig (..), SolverSettings (..))
import Distribution.Client.ProjectPlanning (ComponentTarget (..), Stage (..), SubComponentTarget (..), normaliseUnscopedPackageConfig, nubComponentTargets, setupHsConfigureFlags)
import Distribution.Client.ProjectPlanning.Types (ElaboratedComponent (..), ElaboratedConfiguredPackage (..), ElaboratedPackageOrComponent (..))
import Distribution.Client.Types (ConfiguredId (..), GenericReadyPackage (..))
import Distribution.Simple.Compiler
import Distribution.Simple.Configure (computeEffectiveProfiling)
import Distribution.Simple.Flag
import Distribution.Simple.Setup (ConfigFlags (..), emptyConfigFlags)
import Distribution.Types.ComponentName
import Distribution.Package (PackageIdentifier (..), mkComponentId, newSimpleUnitId, unsafeMkDefUnitId)
import Distribution.Types.Flag (mkFlagAssignment, mkFlagName)
import Distribution.Types.LibraryName
import qualified Distribution.Types.LocalBuildConfig as LBC
import Distribution.Types.PackageName (mkPackageName)
import Distribution.Version (mkVersion)
import Distribution.System (buildPlatform)
import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ testGroup "Build Target Tests" buildTargetTests
  , testGroup "Qualified target package hash" packageHashTargetTests
  , testGroup "Intrinsic profiling configuration layers" profilingBuildOptionsTests
  , testGroup "Install-local target configuration" installPromotionTests
  , testCase "coverage configure flags include canonical sibling unit ids" testCoverageDependencies
  ]

testCoverageDependencies :: Assertion
testCoverageDependencies = do
  let sourceId = PackageIdentifier (mkPackageName "coverage-fixture") (mkVersion [1])
      siblingId = mkComponentId "coverage-fixture-library"
      externalId = PackageIdentifier (mkPackageName "coverage-dependency") (mkVersion [1])
      externalComponentId = mkComponentId "coverage-dependency-library"
      siblingUnitId = newSimpleUnitId siblingId
      externalUnitId = newSimpleUnitId externalComponentId
      component = ElaboratedComponent
        { compSolverName = undefined
        , compComponentName = undefined
        , compLibDependencies = [ (ConfiguredId sourceId Nothing siblingId, False), (ConfiguredId externalId Nothing externalComponentId, False) ]
        , compLinkedLibDependencies = [ DefiniteUnitId (unsafeMkDefUnitId siblingUnitId), DefiniteUnitId (unsafeMkDefUnitId externalUnitId), IndefFullUnitId siblingId Map.empty ]
        , compInstantiatedWith = undefined
        , compLinkedInstantiatedWith = undefined
        , compExeDependencies = undefined
        , compPkgConfigDependencies = undefined
        , compExeDependencyPaths = undefined
        , compOrderLibDependencies = undefined
        }
      elab = ElaboratedConfiguredPackage
        { elabUnitId = undefined
        , elabComponentId = undefined
        , elabIsCanonical = undefined
        , elabPkgSourceId = sourceId
        , elabModuleShape = undefined
        , elabFlagAssignment = undefined
        , elabFlagDefaults = undefined
        , elabPkgDescription = undefined
        , elabGPkgDescription = undefined
        , elabPkgSourceLocation = undefined
        , elabPkgSourceHash = undefined
        , elabIsSourcePackage = undefined
        , elabIsSourcePackageClosure = undefined
        , elabEnabledSpec = undefined
        , elabStanzasAvailable = undefined
        , elabStanzasRequested = undefined
        , elabStage = Host
        , elabToolchain = undefined
        , elabPackageDbs = undefined
        , elabSetupPackageDBStack = undefined
        , elabBuildPackageDBStack = []
        , elabRegisterPackageDBStack = undefined
        , elabPkgDescriptionOverride = undefined
        , elabBuildOptions = testBuildOptions
        , elabDumpBuildInfo = undefined
        , elabProgramPaths = undefined
        , elabProgramArgs = undefined
        , elabProgramPathExtra = undefined
        , elabConfiguredPrograms = undefined
        , elabConfigureScriptArgs = undefined
        , elabExtraLibDirs = undefined
        , elabExtraLibDirsStatic = undefined
        , elabExtraFrameworkDirs = undefined
        , elabExtraIncludeDirs = undefined
        , elabProgPrefix = undefined
        , elabProgSuffix = undefined
        , elabInstallDirs = undefined
        , elabHaddockHoogle = undefined
        , elabHaddockHtml = undefined
        , elabHaddockHtmlLocation = undefined
        , elabHaddockForeignLibs = undefined
        , elabHaddockForHackage = undefined
        , elabHaddockExecutables = undefined
        , elabHaddockTestSuites = undefined
        , elabHaddockBenchmarks = undefined
        , elabHaddockInternal = undefined
        , elabHaddockCss = undefined
        , elabHaddockLinkedSource = undefined
        , elabHaddockQuickJump = undefined
        , elabHaddockHscolourCss = undefined
        , elabHaddockContents = undefined
        , elabHaddockIndex = undefined
        , elabHaddockBaseUrl = undefined
        , elabHaddockResourcesDir = undefined
        , elabHaddockOutputDir = undefined
        , elabHaddockUseUnicode = undefined
        , elabTestMachineLog = undefined
        , elabTestHumanLog = undefined
        , elabTestShowDetails = undefined
        , elabTestKeepTix = undefined
        , elabTestWrapper = undefined
        , elabTestFailWhenNoTestSuites = undefined
        , elabTestTestOptions = undefined
        , elabBenchmarkOptions = undefined
        , elabSetupScriptStyle = undefined
        , elabSetupScriptCliVersion = undefined
        , elabConfigureTargets = undefined
        , elabBuildTargets = undefined
        , elabTestTargets = undefined
        , elabBenchTargets = undefined
        , elabReplTarget = undefined
        , elabHaddockTargets = undefined
        , elabBuildHaddocks = undefined
        , elabPkgOrComp = ElabComponent component
        }
  config <-
    setupHsConfigureFlags
      (\_ -> pure (error "unexpected package database path"))
      (ReadyPackage elab)
      mempty
  case flagToMaybe (configCoverageFor config) of
    Just unitIds -> unitIds @?= [siblingUnitId]
    Nothing -> assertFailure "component coverage omitted its canonical sibling library id"


testBuildOptions :: LBC.BuildOptions
testBuildOptions =
  LBC.BuildOptions
    { LBC.withVanillaLib = True
    , LBC.withProfLib = False
    , LBC.withProfLibShared = False
    , LBC.withSharedLib = False
    , LBC.withStaticLib = False
    , LBC.withBytecodeLib = False
    , LBC.withDynExe = False
    , LBC.withFullyStaticExe = False
    , LBC.withProfExe = False
    , LBC.withProfLibDetail = ProfDetailDefault
    , LBC.withProfExeDetail = ProfDetailDefault
    , LBC.withOptimization = NormalOptimisation
    , LBC.withDebugInfo = NormalDebugInfo
    , LBC.withGHCiLib = False
    , LBC.splitSections = False
    , LBC.splitObjs = False
    , LBC.stripExes = False
    , LBC.stripLibs = False
    , LBC.exeCoverage = True
    , LBC.libCoverage = False
    , LBC.relocatable = False
    }

packageHashTargetTests :: [TestTree]
packageHashTargetTests =
  [ testCase "package hash renders canonical target name and retains compiler build ABI" $ do
      let machine = packageHash "x86_64-unknown-linux-gnu" "build-abi"
          profiling = packageHash "x86_64-unknown-linux-gnu-prof" "build-abi"
          otherBuildAbi = packageHash "x86_64-unknown-linux-gnu" "other-build-abi"
          rendered = LBS.unpack (renderPackageHashInputs machine)
          profRendered = LBS.unpack (renderPackageHashInputs profiling)
      assertBool "hash input must label the canonical target name" $
        "target-name: x86_64-unknown-linux-gnu\n" `isInfixOf` rendered
      assertBool "hash input must preserve the compiler build ABI" $
        "compilerabi: build-abi" `isInfixOf` rendered
      assertBool "different target identities must change package hash input" $
        rendered /= profRendered
      assertBool "target identity must affect the package hash" $
        hashPackageHashInputs machine /= hashPackageHashInputs profiling
      assertBool "build ABI must remain an independent package hash input" $
        "compilerabi: other-build-abi" `isInfixOf` LBS.unpack (renderPackageHashInputs otherBuildAbi)
          && hashPackageHashInputs machine /= hashPackageHashInputs otherBuildAbi
      let armTargets =
            [ "arm-unknown-linux-gnueabi"
            , "arm-unknown-linux-gnueabihf"
            , "arm-unknown-linux-gnueabi-prof"
            , "arm-unknown-linux-gnueabihf-prof"
            ]
          armHashes = map (hashPackageHashInputs . (`packageHash` "build-abi")) armTargets
          armInputs = map (renderPackageHashInputs . (`packageHash` "build-abi")) armTargets
      length (nub armInputs) @?= 4
      length (nub armHashes) @?= 4
  ]
  where
    packageHash targetName buildAbi =
      PackageHashInputs
        { pkgHashPkgId = PackageIdentifier (mkPackageName "target-test") (mkVersion [1])
        , pkgHashComponent = Nothing
        , pkgHashSourceHash = hashValue mempty
        , pkgHashPkgConfigDeps = mempty
        , pkgHashLibDeps = mempty
        , pkgHashExeDeps = mempty
        , pkgHashOtherConfig =
            PackageHashConfigInputs
              { pkgHashCompilerId = CompilerId GHC (mkVersion [9, 14, 1])
              , pkgHashCompilerABI = AbiTag buildAbi
              , pkgHashTargetName = targetName
              , pkgHashPlatform = buildPlatform
              , pkgHashFlagAssignment = mempty
              , pkgHashConfigureScriptArgs = []
              , pkgHashVanillaLib = True
              , pkgHashSharedLib = False
              , pkgHashDynExe = False
              , pkgHashFullyStaticExe = False
              , pkgHashGHCiLib = False
              , pkgHashProfLib = False
              , pkgHashProfExe = False
              , pkgHashProfLibDetail = ProfDetailDefault
              , pkgHashProfExeDetail = ProfDetailDefault
              , pkgHashBytecodeLib = False
              , pkgHashCoverage = False
              , pkgHashOptimization = NormalOptimisation
              , pkgHashSplitObjs = False
              , pkgHashSplitSections = False
              , pkgHashStripLibs = False
              , pkgHashStripExes = True
              , pkgHashDebugInfo = NormalDebugInfo
              , pkgHashProgramArgs = mempty
              , pkgHashExtraLibDirs = []
              , pkgHashExtraLibDirsStatic = []
              , pkgHashExtraFrameworkDirs = []
              , pkgHashExtraIncludeDirs = []
              , pkgHashProgPrefix = Nothing
              , pkgHashProgSuffix = Nothing
              , pkgHashPackageDbs = []
              , pkgHashDocumentation = False
              , pkgHashHaddockHoogle = False
              , pkgHashHaddockHtml = False
              , pkgHashHaddockHtmlLocation = Nothing
              , pkgHashHaddockForeignLibs = False
              , pkgHashHaddockExecutables = False
              , pkgHashHaddockTestSuites = False
              , pkgHashHaddockBenchmarks = False
              , pkgHashHaddockInternal = False
              , pkgHashHaddockCss = Nothing
              , pkgHashHaddockLinkedSource = False
              , pkgHashHaddockQuickJump = False
              , pkgHashHaddockContents = Nothing
              , pkgHashHaddockIndex = Nothing
              , pkgHashHaddockBaseUrl = Nothing
              , pkgHashHaddockResourcesDir = Nothing
              , pkgHashHaddockOutputDir = Nothing
              , pkgHashHaddockUseUnicode = False
              }
        }

installPromotionTests :: [TestTree]
installPromotionTests =
  [ testCase "promotion retains its origin and genuine package settings" $ do
      projectConfigSpecificPackage promoted @?= projectConfigSpecificPackage original
      getMapMappend (projectConfigInstallPackages promoted) @?= Map.singleton name local
  , testCase "native Build does not inherit promoted target profiling or details" $ do
      let opts = forStage Build promoted
      effectiveProfiling opts @?= (False, False, False)
      packageConfigProfDetail opts @?= Flag ProfDetailNone
      packageConfigProfLibDetail opts @?= Flag ProfDetailNone
      packageConfigOptimization opts @?= Flag MaximumOptimisation
  , testCase "Host install flags retain CLI precedence over package settings" $ do
      let opts = forStage Host promoted
      effectiveProfiling opts @?= (True, False, True)
      packageConfigProfDetail opts @?= Flag ProfDetailToplevelFunctions
      packageConfigProfLibDetail opts @?= Flag ProfDetailTopLate
  , testCase "explicit package profiling still applies to the native Build copy" $ do
      let explicitConfig = original{projectConfigSpecificPackage = MapMappend (Map.singleton name mempty{packageConfigProf = Flag True})}
          opts = forStage Build (addLocalConfigToPkgs explicitConfig [name])
      effectiveProfiling opts @?= (True, False, True)
  , testCase "explicit Build-stage profiling survives the install overlay" $ do
      let scoped = mempty{packageConfigProf = Flag True, packageConfigProfDetail = Flag ProfDetailAllFunctions}
          config = promoted{projectConfigSpecificPackage = mempty, projectConfigStagePackages = MapMappend (Map.singleton Build scoped)}
          opts = forStage Build config
      effectiveProfiling opts @?= (True, False, True)
      packageConfigProfDetail opts @?= Flag ProfDetailAllFunctions
  , testCase "ordinary targets retain install profiling requests for validation" $
      effectiveProfiling (normaliseUnscopedPackageConfig Build ordinaryCompiler ordinaryCompiler local)
        @?= (True, False, True)
  , testCase "unrelated packages do not receive install-local options" $
      Map.lookup (mkPackageName "unselected") (getMapMappend (projectConfigInstallPackages promoted)) @?= Nothing
  , testCase "non-staged package lookup keeps the previous merged settings" $ do
      lookupLocalPackageConfig packageConfigOptimization promoted name @?= Flag MaximumOptimisation
      let previous = original{projectConfigSpecificPackage = projectConfigSpecificPackage original <> MapMappend (Map.singleton name local)}
      lookupLocalPackageConfig packageConfigProgramArgs promoted name
        @?= lookupLocalPackageConfig packageConfigProgramArgs previous name
  , testCase "solver sees promoted package flags and profiling-shared constraints" $ do
      let assignment = mkFlagAssignment [(mkFlagName "selected", True)]
          localSolver = mempty{packageConfigFlagAssignment = assignment, packageConfigProfShared = Flag True}
          config = addLocalConfigToPkgs mempty{projectConfigLocalPackages = localSolver} [name]
          -- Install no longer treats source distributions as local packages.
          settings = resolveSolverSettings config{projectConfigLocalPackages = mempty}
      Map.lookup name (solverSettingFlagAssignments settings) @?= Just assignment
      length (solverSettingConstraints settings) @?= 1
  , testCase "promoted details survive cache serialization and change its input" $ do
      let restored = decode (encode promoted) :: ProjectConfig
      restored @?= promoted
      forStage Build restored @?= forStage Build promoted
      forStage Host restored @?= forStage Host promoted
      let changed = promoted{projectConfigInstallPackages = MapMappend (Map.singleton name local{packageConfigProfDetail = Flag ProfDetailAllFunctions})}
      assertBool "profiling detail must be part of the monitored configuration" (encode promoted /= encode changed)
  ]
  where
    name = mkPackageName "selected"
    local =
      mempty
        { packageConfigProf = Flag True
        , packageConfigProfLib = Flag True
        , packageConfigProfExe = Flag True
        , packageConfigProfShared = Flag False
        , packageConfigProfDetail = Flag ProfDetailToplevelFunctions
        , packageConfigProfLibDetail = Flag ProfDetailTopLate
        , packageConfigOptimization = Flag MaximumOptimisation
        , packageConfigProgramArgs = MapMappend (Map.singleton "ghc" ["-DLOCAL"])
        }
    explicit =
      mempty
        { packageConfigProf = Flag False
        , packageConfigProfDetail = Flag ProfDetailNone
        , packageConfigProfLibDetail = Flag ProfDetailNone
        , packageConfigOptimization = Flag NoOptimisation
        , packageConfigProgramArgs = MapMappend (Map.singleton "ghc" ["-DPACKAGE"])
        }
    original =
      mempty
        { projectConfigLocalPackages = local
        , projectConfigSpecificPackage = MapMappend (Map.singleton name explicit)
        }
    promoted = addLocalConfigToPkgs original [name]
    forStage stage config =
      Map.findWithDefault mempty stage (getMapMappend (projectConfigStagePackages config))
        <> Map.findWithDefault mempty name (getMapMappend (projectConfigSpecificPackage config))
        <> normaliseUnscopedPackageConfig
          stage
          ordinaryCompiler
          profilingCompiler
          (Map.findWithDefault mempty name (getMapMappend (projectConfigInstallPackages config)))

-- Check that scoped options retain Cabal's Setup flag defaults after the
-- unscoped configuration layers have been normalized.
profilingBuildOptionsTests :: [TestTree]
profilingBuildOptionsTests =
  [ testCase "unscoped target profiling leaves ordinary build defaults" $ do
      let normalized = normalize Build unscoped
      effectiveProfiling normalized @?= (False, False, False)
      packageConfigProfLib normalized @?= NoFlag
      packageConfigProfExe normalized @?= NoFlag
      packageConfigProfShared normalized @?= NoFlag
      packageConfigProfDetail normalized @?= NoFlag
      packageConfigProfLibDetail normalized @?= NoFlag
      packageConfigSharedLib normalized @?= Flag True
  , testCase "explicit build profiling request survives for target validation" $ do
      let buildScoped = mempty{packageConfigProf = Flag True}
      effectiveProfiling (normalize Build unscoped <> buildScoped) @?= (True, False, True)
  , testCase "explicit package profiling request survives for target validation" $ do
      let perPackage = mempty{packageConfigProf = Flag True}
      effectiveProfiling (normalize Build unscoped <> perPackage) @?= (True, False, True)
  , testCase "scoped dynamic profiling retains Setup shared-library defaults" $ do
      let buildScoped =
            mempty
              { packageConfigProf = Flag True
              , packageConfigDynExe = Flag True
              }
      effectiveProfiling (normalize Build unscoped <> buildScoped) @?= (False, True, True)
  , testCase "explicit build library opt-out remains effective" $ do
      let buildScoped =
            mempty
              { packageConfigProf = Flag True
              , packageConfigProfLib = Flag False
              }
      effectiveProfiling (normalize Build unscoped <> buildScoped) @?= (False, False, True)
  , testCase "host profiling options are preserved" $
      normalize Host unscoped @?= unscoped
  , testCase "explicit build detail survives for target validation" $ do
      let scoped = mempty{packageConfigProfDetail = Flag ProfDetailToplevelFunctions}
      packageConfigProfDetail (normalize Build unscoped <> scoped)
        @?= Flag ProfDetailToplevelFunctions
  , testCase "all explicit Build profiling fields survive target scoping" $ do
      let scoped =
            mempty
              { packageConfigProf = Flag True
              , packageConfigProfLib = Flag False
              , packageConfigProfExe = Flag True
              , packageConfigProfShared = Flag True
              , packageConfigProfDetail = Flag ProfDetailAllFunctions
              , packageConfigProfLibDetail = Flag ProfDetailTopLate
              }
          result = normalize Build unscoped <> scoped
      map
        (\f -> f result)
        [ packageConfigProf
        , packageConfigProfLib
        , packageConfigProfExe
        , packageConfigProfShared
        ]
        @?= [Flag True, Flag False, Flag True, Flag True]
      packageConfigProfDetail result @?= Flag ProfDetailAllFunctions
      packageConfigProfLibDetail result @?= Flag ProfDetailTopLate
  , testCase "ordinary cross builds retain existing unscoped options" $
      normaliseUnscopedPackageConfig Build ordinaryCompiler ordinaryCompiler unscoped @?= unscoped
  , testCase "matching intrinsic build ABI retains its options" $
      normaliseUnscopedPackageConfig Build profilingCompiler profilingCompiler unscoped @?= unscoped
  ]
  where
    normalize stage = normaliseUnscopedPackageConfig stage ordinaryCompiler profilingCompiler
    -- An explicit library opt-out in a target layer must not become a
    -- synthetic opt-out in an independently scoped build configuration.
    unscoped =
      mempty
        { packageConfigProf = Flag True
        , packageConfigProfLib = Flag False
        , packageConfigProfExe = Flag True
        , packageConfigProfShared = Flag False
        , packageConfigSharedLib = Flag True
        , packageConfigProfDetail = Flag ProfDetailAllFunctions
        , packageConfigProfLibDetail = Flag ProfDetailTopLate
        }

effectiveProfiling :: PackageConfig -> (Bool, Bool, Bool)
effectiveProfiling config =
  computeEffectiveProfiling
    emptyConfigFlags
      { configProf = packageConfigProf config
      , configProfLib = packageConfigProfLib config
      , configProfExe = packageConfigProfExe config
      , configProfShared = packageConfigProfShared config
      , configDynExe = packageConfigDynExe config
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
    , compilerProperties = Map.empty
    }

profilingCompiler :: Compiler
profilingCompiler =
  ordinaryCompiler
    { compilerProperties = Map.singleton "Target ABI" "profiling"
    }

-- ----------------------------------------------------------------------------
-- Build Target Tests
-- ----------------------------------------------------------------------------

buildTargetTests :: [TestTree]
buildTargetTests =
  [ testGroup "nubComponentTargets" nubComponentTargetsTests
  ]

nubComponentTargetsTests :: [TestTree]
nubComponentTargetsTests =
  [ testCase "Works on empty list" $
      nubComponentTargets [] @?= ([] :: [(ComponentTarget, NonEmpty Int)])
  , testCase "Merges targets to same component" $
      nubComponentTargets
        [ (mainLibModuleTarget, 1 :: Int)
        , (mainLibFileTarget, 2)
        ]
        @?= [(mainLibWholeCompTarget, 1 :| [2])]
  , testCase "Merges whole component targets" $
      nubComponentTargets [(mainLibFileTarget, 2), (mainLibWholeCompTarget, 1 :: Int)]
        @?= [(mainLibWholeCompTarget, 2 :| [1])]
  , testCase "Don't merge unrelated targets" $
      nubComponentTargets
        [ (mainLibWholeCompTarget, 1 :: Int)
        , (exeWholeCompTarget, 2)
        ]
        @?= [(mainLibWholeCompTarget, pure 1), (exeWholeCompTarget, pure 2)]
  , testCase "Merge multiple related targets" $
      nubComponentTargets
        [ (mainLibWholeCompTarget, 1 :: Int)
        , (mainLibModuleTarget, 4)
        , (exeWholeCompTarget, 2)
        , (exeFileTarget, 3)
        ]
        @?= [(mainLibWholeCompTarget, 1 :| [4]), (exeWholeCompTarget, 2 :| [3])]
  , testCase "Merge related targets, don't merge unrelated ones" $
      nubComponentTargets
        [ (mainLibFileTarget, 1 :: Int)
        , (mainLibModuleTarget, 4)
        , (exeWholeCompTarget, 2)
        , (exeFileTarget, 3)
        , (exe2FileTarget, 5)
        ]
        @?= [ (mainLibWholeCompTarget, 1 :| [4])
            , (exeWholeCompTarget, 2 :| [3])
            , (exe2WholeCompTarget, 5 :| [])
            ]
  ]

-- ----------------------------------------------------------------------------
-- Utils
-- ----------------------------------------------------------------------------

mainLibWholeCompTarget :: ComponentTarget
mainLibWholeCompTarget = ComponentTarget (CLibName LMainLibName) WholeComponent

mainLibModuleTarget :: ComponentTarget
mainLibModuleTarget = ComponentTarget (CLibName LMainLibName) (ModuleTarget "Lib")

mainLibFileTarget :: ComponentTarget
mainLibFileTarget = ComponentTarget (CLibName LMainLibName) (FileTarget "./Lib.hs")

exeWholeCompTarget :: ComponentTarget
exeWholeCompTarget = ComponentTarget (CExeName "exe") WholeComponent

exeFileTarget :: ComponentTarget
exeFileTarget = ComponentTarget (CExeName "exe") (FileTarget "./Main.hs")

exe2WholeCompTarget :: ComponentTarget
exe2WholeCompTarget = ComponentTarget (CExeName "exe2") WholeComponent

exe2FileTarget :: ComponentTarget
exe2FileTarget = ComponentTarget (CExeName "exe2") (FileTarget "./Main2.hs")
