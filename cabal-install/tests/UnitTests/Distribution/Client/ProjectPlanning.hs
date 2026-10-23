{-# LANGUAGE OverloadedStrings #-}

module UnitTests.Distribution.Client.ProjectPlanning (tests) where

import Data.Binary (decode, encode)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as Map
import Distribution.Backpack (OpenUnitId (..))
import Distribution.Backpack.ModuleShape (emptyModuleShape)
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.CmdInstall (addLocalConfigToPkgs)
import Distribution.Client.ProjectConfig (lookupLocalPackageConfig, resolveSolverSettings)
import Distribution.Client.ProjectConfig.Types (MapMappend (..), PackageConfig (..), ProjectConfig (..), ProjectConfigShared (..), SolverSettings (..))
import Distribution.Client.ProjectPlanning (ComponentTarget (..), SubComponentTarget (..), determineCoverageFor, normaliseUnscopedPackageConfig, nubComponentTargets, projectPackageDbsFor, sameCompiler)
import Distribution.Client.ProjectPlanning.Types
import Distribution.Client.Toolchain (Staged (..), Toolchain (..))
import Distribution.Compiler (CompilerFlavor (..))
import Distribution.Package (PackageIdentifier (..), mkComponentId, mkPackageName, newSimpleUnitId, unsafeMkDefUnitId)
import Distribution.Simple.Compiler (AbiTag (..), Compiler (..), CompilerId (..), PackageDBX (..))
import Distribution.Simple.Configure (computeEffectiveProfiling)
import Distribution.Simple.Flag
import Distribution.Simple.Program.Db (defaultProgramDb, userSpecifyPath)
import Distribution.Simple.Setup (emptyConfigFlags)
import Distribution.Solver.Types.Settings (IndependentGoals (..))
import Distribution.Solver.Types.Stage (Stage (..), prevStage)
import Distribution.System (Arch (..), OS (..), Platform (..))
import Distribution.Types.ComponentName
import Distribution.Types.Flag (mkFlagAssignment, mkFlagName)
import Distribution.Types.LibraryName
import Distribution.Version (mkVersion)
import qualified Distribution.Types.LocalBuildConfig as LBC
import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ testGroup "Build Target Tests" buildTargetTests
  , testGroup "sameCompiler" sameCompilerTests
  , testGroup "projectPackageDbsFor" projectPackageDbsForTests
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
      emptyElab :: ElaboratedConfiguredPackage
      emptyElab = ElaboratedConfiguredPackage
        { elabUnitId = error "emptyElab.elabUnitId"
        , elabComponentId = error "emptyElab.elabComponentId"
        , elabInstantiatedWith = error "emptyElab.elabInstantiatedWith"
        , elabLinkedInstantiatedWith = error "emptyElab.elabLinkedInstantiatedWith"
        , elabIsCanonical = error "emptyElab.elabIsCanonical"
        , elabPkgSourceId = error "emptyElab.elabPkgSourceId"
        , elabStage = error "emptyElab.elabStage"
        , elabModuleShape = error "emptyElab.elabModuleShape"
        , elabFlagAssignment = error "emptyElab.elabFlagAssignment"
        , elabFlagDefaults = error "emptyElab.elabFlagDefaults"
        , elabPkgDescription = error "emptyElab.elabPkgDescription"
        , elabGPkgDescription = error "emptyElab.elabGPkgDescription"
        , elabPkgSourceLocation = error "emptyElab.elabPkgSourceLocation"
        , elabPkgSourceHash = error "emptyElab.elabPkgSourceHash"
        , elabLocalToProject = error "emptyElab.elabLocalToProject"
        , elabBuildStyle = error "emptyElab.elabBuildStyle"
        , elabEnabledSpec = error "emptyElab.elabEnabledSpec"
        , elabStanzasAvailable = error "emptyElab.elabStanzasAvailable"
        , elabStanzasRequested = error "emptyElab.elabStanzasRequested"
        , elabPackageDbs = error "emptyElab.elabPackageDbs"
        , elabSetupPackageDBStack = error "emptyElab.elabSetupPackageDBStack"
        , elabBuildPackageDBStack = error "emptyElab.elabBuildPackageDBStack"
        , elabRegisterPackageDBStack = error "emptyElab.elabRegisterPackageDBStack"
        , elabInplaceSetupPackageDBStack = error "emptyElab.elabInplaceSetupPackageDBStack"
        , elabInplaceBuildPackageDBStack = error "emptyElab.elabInplaceBuildPackageDBStack"
        , elabInplaceRegisterPackageDBStack = error "emptyElab.elabInplaceRegisterPackageDBStack"
        , elabPkgDescriptionOverride = error "emptyElab.elabPkgDescriptionOverride"
        , elabBuildOptions = error "emptyElab.elabBuildOptions"
        , elabDumpBuildInfo = error "emptyElab.elabDumpBuildInfo"
        , elabProgramPaths = error "emptyElab.elabProgramPaths"
        , elabProgramArgs = error "emptyElab.elabProgramArgs"
        , elabProgramPathExtra = error "emptyElab.elabProgramPathExtra"
        , elabConfiguredPrograms = error "emptyElab.elabConfiguredPrograms"
        , elabConfigureScriptArgs = error "emptyElab.elabConfigureScriptArgs"
        , elabExtraLibDirs = error "emptyElab.elabExtraLibDirs"
        , elabExtraLibDirsStatic = error "emptyElab.elabExtraLibDirsStatic"
        , elabExtraFrameworkDirs = error "emptyElab.elabExtraFrameworkDirs"
        , elabExtraIncludeDirs = error "emptyElab.elabExtraIncludeDirs"
        , elabProgPrefix = error "emptyElab.elabProgPrefix"
        , elabProgSuffix = error "emptyElab.elabProgSuffix"
        , elabInstallDirs = error "emptyElab.elabInstallDirs"
        , elabHaddockHoogle = error "emptyElab.elabHaddockHoogle"
        , elabHaddockHtml = error "emptyElab.elabHaddockHtml"
        , elabHaddockHtmlLocation = error "emptyElab.elabHaddockHtmlLocation"
        , elabHaddockForeignLibs = error "emptyElab.elabHaddockForeignLibs"
        , elabHaddockForHackage = error "emptyElab.elabHaddockForHackage"
        , elabHaddockExecutables = error "emptyElab.elabHaddockExecutables"
        , elabHaddockTestSuites = error "emptyElab.elabHaddockTestSuites"
        , elabHaddockBenchmarks = error "emptyElab.elabHaddockBenchmarks"
        , elabHaddockInternal = error "emptyElab.elabHaddockInternal"
        , elabHaddockCss = error "emptyElab.elabHaddockCss"
        , elabHaddockLinkedSource = error "emptyElab.elabHaddockLinkedSource"
        , elabHaddockQuickJump = error "emptyElab.elabHaddockQuickJump"
        , elabHaddockHscolourCss = error "emptyElab.elabHaddockHscolourCss"
        , elabHaddockContents = error "emptyElab.elabHaddockContents"
        , elabHaddockIndex = error "emptyElab.elabHaddockIndex"
        , elabHaddockBaseUrl = error "emptyElab.elabHaddockBaseUrl"
        , elabHaddockResourcesDir = error "emptyElab.elabHaddockResourcesDir"
        , elabHaddockOutputDir = error "emptyElab.elabHaddockOutputDir"
        , elabHaddockUseUnicode = error "emptyElab.elabHaddockUseUnicode"
        , elabTestMachineLog = error "emptyElab.elabTestMachineLog"
        , elabTestHumanLog = error "emptyElab.elabTestHumanLog"
        , elabTestShowDetails = error "emptyElab.elabTestShowDetails"
        , elabTestKeepTix = error "emptyElab.elabTestKeepTix"
        , elabTestWrapper = error "emptyElab.elabTestWrapper"
        , elabTestFailWhenNoTestSuites = error "emptyElab.elabTestFailWhenNoTestSuites"
        , elabTestTestOptions = error "emptyElab.elabTestTestOptions"
        , elabBenchmarkOptions = error "emptyElab.elabBenchmarkOptions"
        , elabSetupScriptStyle = error "emptyElab.elabSetupScriptStyle"
        , elabSetupScriptCliVersion = error "emptyElab.elabSetupScriptCliVersion"
        , elabConfigureTargets = error "emptyElab.elabConfigureTargets"
        , elabBuildTargets = error "emptyElab.elabBuildTargets"
        , elabTestTargets = error "emptyElab.elabTestTargets"
        , elabBenchTargets = error "emptyElab.elabBenchTargets"
        , elabReplTarget = error "emptyElab.elabReplTarget"
        , elabHaddockTargets = error "emptyElab.elabHaddockTargets"
        , elabBuildHaddocks = error "emptyElab.elabBuildHaddocks"
        , elabPkgOrComp = error "emptyElab.elabPkgOrComp"
        }

      emptyComp :: ElaboratedComponent
      emptyComp = ElaboratedComponent
        { compSolverName = error "emptyComp.compSolverName"
        , compComponentName = error "emptyComp.compComponentName"
        , compLibDependencies = error "emptyComp.compLibDependencies"
        , compLinkedLibDependencies = error "emptyComp.compLinkedLibDependencies"
        , compExeDependencies = error "emptyComp.compExeDependencies"
        , compPkgConfigDependencies = error "emptyComp.compPkgConfigDependencies"
        , compExeDependencyPaths = error "emptyComp.compExeDependencyPaths"
        , compOrderLibDependencies = error "emptyComp.compOrderLibDependencies"
        }
      component = emptyComp
        { compLibDependencies =
            [ (ConfiguredId sourceId Nothing siblingId, False)
            , (ConfiguredId externalId Nothing externalComponentId, False)
            ]
        , compLinkedLibDependencies =
            [ DefiniteUnitId (unsafeMkDefUnitId siblingUnitId)
            , DefiniteUnitId (unsafeMkDefUnitId externalUnitId)
            , IndefFullUnitId siblingId Map.empty
            ]
        }
      elab = emptyElab
        { elabPkgOrComp = ElabComponent component
        , elabPkgSourceId = sourceId
        , elabStage = Host
        }
      mkLib pkgId unitId = emptyElab
        { elabUnitId = unitId
        , elabPkgSourceId = pkgId
        , elabLocalToProject = True
        , elabModuleShape = emptyModuleShape
        , elabStage = Host
        , elabPkgOrComp =
            ElabComponent
              emptyComp
                { compComponentName = Just (CLibName LMainLibName)
                , compExeDependencies = []
                , compOrderLibDependencies = []
                }
        }
      plan =
        InstallPlan.new (IndependentGoals False) $
          Graph.fromDistinctList
            [ InstallPlan.Configured (mkLib sourceId siblingUnitId)
            , InstallPlan.Configured (mkLib externalId externalUnitId)
            ]
  determineCoverageFor elab plan @?= Flag [siblingUnitId]

-- ----------------------------------------------------------------------------
-- projectPackageDbsFor
--
-- A package database is only readable by the compiler that wrote it, so the
-- project's 'package-dbs' must not reach a distinct build toolchain. The
-- subtle case is the other one: setup scripts are built at 'prevStage' of
-- the package's own stage, and a non-cross build has no build stage, so
-- 'prevStage' must keep them at 'Host' and the project's databases must
-- still apply there.
-- ----------------------------------------------------------------------------

projectPackageDbsForTests :: [TestTree]
projectPackageDbsForTests =
  [ testCase "host stage keeps the project databases" $
      projectPackageDbsFor config Host @?= configured
  , testCase "the build stage gets none of them" $
      projectPackageDbsFor config Build @?= []
  , testCase "a distinct build toolchain puts setup scripts at the build stage" $
      prevStage cross Host @?= Build
  , testCase "the build stage is the earliest and clamps" $
      prevStage cross Build @?= Build
  , testCase "without a build toolchain setup scripts stay at the host stage" $
      -- This is what a setup script of a host-stage package asks for in an
      -- ordinary build, and it must still see the project's databases.
      projectPackageDbsFor config (prevStage nonCross Host) @?= configured
  ]
  where
    configured = [Just (SpecificPackageDB "/somewhere/package.db")]

    config = (mempty :: ProjectConfigShared){projectConfigPackageDBs = configured}

    -- 'prevStage' only looks at whether there is a build stage; which
    -- compilers they are does not matter here.
    nonCross = Staged (toolchain ghc912 linuxX86_64) Nothing
    cross = Staged (toolchain ghc912 linuxX86_64) (Just (toolchain ghc910 linuxAArch64))

-- ----------------------------------------------------------------------------
-- sameCompiler
--
-- 'configureToolchains' uses 'sameCompiler' to decide whether a requested
-- build compiler is really a second stage. Getting this wrong is expensive in
-- both directions: a false negative solves and builds every shared dependency
-- twice under indistinguishable unit ids, and a false positive silently drops
-- a genuine cross-compilation setup back to a single stage.
-- ----------------------------------------------------------------------------

sameCompilerTests :: [TestTree]
sameCompilerTests =
  [ testCase "a toolchain is the same as itself" $
      sameCompiler (toolchain ghc912 linuxX86_64) (toolchain ghc912 linuxX86_64) @?= True
  , testCase "the same compiler named two ways is the same" $
      -- What GHC's own staged bootstrap does: --with-build-compiler and
      -- --with-compiler given the same compiler, spelled differently. The
      -- ProgramDb differs; nothing that reaches a unit id does.
      sameCompiler
        (toolchain ghc912 linuxX86_64)
        ((toolchain ghc912 linuxX86_64){toolchainProgramDb = userSpecifyPath "ghc" "/usr/bin/ghc" defaultProgramDb})
        @?= True
  , testCase "differing compiler version is not the same" $
      sameCompiler (toolchain ghc912 linuxX86_64) (toolchain ghc910 linuxX86_64) @?= False
  , testCase "differing ABI tag is not the same" $
      sameCompiler
        (toolchain ghc912 linuxX86_64)
        (toolchain ghc912{compilerAbiTag = AbiTag "deadbeef"} linuxX86_64)
        @?= False
  , testCase "differing target platform is not the same" $
      -- The case the store layout also has to separate: one compiler version
      -- targeting two architectures.
      sameCompiler (toolchain ghc912 linuxX86_64) (toolchain ghc912 linuxAArch64) @?= False
  , testCase "differing compiler flavour is not the same" $
      sameCompiler (toolchain ghc912 linuxX86_64) (toolchain ghcjs912 linuxX86_64) @?= False
  ]

toolchain :: Compiler -> Platform -> Toolchain
toolchain c p =
  Toolchain
    { toolchainCompiler = c
    , toolchainPlatform = p
    , toolchainProgramDb = defaultProgramDb
    }

linuxX86_64, linuxAArch64 :: Platform
linuxX86_64 = Platform X86_64 Linux
linuxAArch64 = Platform AArch64 Linux

ghc912, ghc910, ghcjs912 :: Compiler
ghc912 = mkCompiler GHC [9, 12, 2]
ghc910 = mkCompiler GHC [9, 10, 3]
ghcjs912 = mkCompiler GHCJS [9, 12, 2]

mkCompiler :: CompilerFlavor -> [Int] -> Compiler
mkCompiler flavour version =
  Compiler
    { compilerId = CompilerId flavour (mkVersion version)
    , compilerAbiTag = NoAbiTag
    , compilerCompat = []
    , compilerLanguages = []
    , compilerExtensions = []
    , compilerProperties = mempty
    , compilerWiredInUnitIds = Nothing
    }


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
      (decode (encode promoted) :: ProjectConfig) @?= promoted
      let changed = promoted{projectConfigInstallPackages = MapMappend (Map.singleton name local{packageConfigProfDetail = Flag ProfDetailAllFunctions})}
      assertBool "profiling detail must be part of the monitored configuration" (encode promoted /= encode changed)
  ]
  where
    name = mkPackageName "selected"
    local =
      mempty
        { packageConfigProf = Flag True
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
