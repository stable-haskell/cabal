{-# LANGUAGE OverloadedStrings #-}

module UnitTests.Distribution.Client.ProjectPlanning (tests) where

import Data.List.NonEmpty
import qualified Data.Map as Map
import Distribution.Backpack (OpenUnitId (..))
import Distribution.Client.ProjectPlanning (ComponentTarget (..), SubComponentTarget (..), nubComponentTargets, setupHsConfigureFlags)
import Distribution.Client.ProjectPlanning.Types
import Distribution.Client.Types (ConfiguredId (..), GenericReadyPackage (..))
import Distribution.Package (PackageIdentifier (..), mkComponentId, mkPackageName, newSimpleUnitId, unsafeMkDefUnitId)
import qualified Distribution.Simple.Setup as Cabal
import Distribution.Simple.Flag (flagToMaybe)
import qualified Distribution.Types.LocalBuildConfig as LBC
import Distribution.Types.ComponentName
import Distribution.Types.LibraryName
import Distribution.Version (mkVersion)
import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ testGroup "Build Target Tests" buildTargetTests
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
      component =
        (undefined :: ElaboratedComponent)
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
      elab =
        (undefined :: ElaboratedConfiguredPackage)
          { elabPkgOrComp = ElabComponent component
          , elabPkgSourceId = sourceId
          , elabStage = Host
          , elabBuildPackageDBStack = []
          , elabBuildOptions = (undefined :: LBC.BuildOptions){LBC.exeCoverage = True}
          }
  config <-
    setupHsConfigureFlags
      (\_ -> pure (error "unexpected package database path"))
      (ReadyPackage elab)
      mempty
  case flagToMaybe (Cabal.configCoverageFor config) of
    Just unitIds -> unitIds @?= [siblingUnitId]
    Nothing -> assertFailure "component coverage omitted its canonical sibling library id"

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
