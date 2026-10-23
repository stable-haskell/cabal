{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UnitTests.Distribution.Client.Described where

import Distribution.Client.Compat.Prelude
import Test.QuickCheck.Instances.Cabal ()
import UnitTests.Distribution.Client.ArbitraryInstances ()
import UnitTests.Distribution.Client.DescribedInstances ()
import Prelude ()

import Distribution.Described (prettyMatchesDescription, testDescribed)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

import Distribution.Client.BuildReports.Types (InstallOutcome, Outcome)
import Distribution.Client.IndexUtils.ActiveRepos (ActiveRepos)
import Distribution.Client.IndexUtils.IndexState (RepoIndexState, TotalIndexState)
import Distribution.Client.IndexUtils.Timestamp (Timestamp)
import Distribution.Client.Targets
  ( UserConstraint (UserConstraintStaged)
  , UserConstraintQualifier (..)
  , UserQualifier (..)
  )
import Distribution.Client.Types (RepoName)
import Distribution.Client.Types.AllowNewer (RelaxDepSubject, RelaxDeps, RelaxedDep)
import Distribution.Package (mkPackageName)
import Distribution.Solver.Types.PackageConstraint (PackageProperty (..))
import Distribution.Solver.Types.Stage (Stage (..))

tests :: TestTree
tests =
  testGroup
    "Described"
    ( [ testDescribed (Proxy :: Proxy Timestamp)
      , testDescribed (Proxy :: Proxy RepoIndexState)
      , testDescribed (Proxy :: Proxy TotalIndexState)
      , testDescribed (Proxy :: Proxy RepoName)
      , testDescribed (Proxy :: Proxy ActiveRepos)
      , testDescribed (Proxy :: Proxy RelaxDepSubject)
      , testDescribed (Proxy :: Proxy RelaxedDep)
      , testDescribed (Proxy :: Proxy RelaxDeps)
      , testDescribed (Proxy :: Proxy UserConstraint)
      , testDescribed (Proxy :: Proxy InstallOutcome)
      , testDescribed (Proxy :: Proxy Outcome)
      ]
        ++ [ testCase ("UserConstraint.pretty matches grammar: " ++ show constraint) $
            assertBool "pretty output did not match the described grammar" $
              prettyMatchesDescription constraint
           | constraint <- stagedConstraintExamples
           ]
    )
  where
    stagedConstraintExamples =
      [ UserConstraintStaged Build (UserQualified UserQualToplevel package) PackagePropertyInstalled
      , UserConstraintStaged Host (UserAnyQualifier package) PackagePropertySource
      , UserConstraintStaged Build (UserAnySetupQualifier package) PackagePropertySource
      , UserConstraintStaged Host (UserAnyExeQualifier package) PackagePropertySource
      , UserConstraintStaged Build (UserQualified (UserQualSetup package) package) PackagePropertyInstalled
      ]
      where
        package = mkPackageName "Q"
