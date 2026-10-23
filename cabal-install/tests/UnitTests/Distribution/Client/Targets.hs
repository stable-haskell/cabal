module UnitTests.Distribution.Client.Targets
  ( tests
  ) where

import Distribution.Client.Targets
  ( UserConstraint (..)
  , UserConstraintQualifier (..)
  , UserQualifier (..)
  , readUserConstraint
  )
import Distribution.Package (PackageIdentifier (..), mkPackageName)
import Distribution.Client.Types.PackageSpecifier (PackageSpecifier (..), pkgSpecifierConstraints)
import Distribution.PackageDescription (mkFlagAssignment, mkFlagName)
import Distribution.Version (anyVersion, mkVersion, thisVersion)

import Distribution.Parsec (explicitEitherParsec, parsec, parsecCommaList)
import Distribution.Pretty (prettyShow)

import Distribution.Solver.Types.OptionalStanza (OptionalStanza (..))
import Distribution.Solver.Types.ConstraintSource (ConstraintSource (ConstraintSourceUserTarget))
import Distribution.Solver.Types.LabeledPackageConstraint (LabeledPackageConstraint (..))
import Distribution.Solver.Types.PackageConstraint (ConstraintQualifier (..), ConstraintScope (..), PackageConstraint (..), PackageProperty (..), constraintScopeMatches)
import Distribution.Solver.Types.PackagePath (Namespace (..), PackagePath (..), Qualified (..), Qualifier (..))
import Distribution.Solver.Types.Stage (Stage (..), showStage)

import Test.Tasty
import Test.Tasty.HUnit

import Data.List (intercalate)

-- Helper function: makes a test group by mapping each element
-- of a list to a test case.
makeGroup :: String -> (a -> Assertion) -> [a] -> TestTree
makeGroup name f xs =
  testGroup name $
    zipWith testCase (map show [0 :: Integer ..]) (map f xs)

tests :: [TestTree]
tests =
  [ testCase "local source versions constrain their own target namespace" $ do
      let name = mkPackageName "local-package"
          other = mkPackageName "other-root"
          version = mkVersion [2]
          scope = ConstraintScope (Just Host) (ScopeTarget name)
          goal stage namespace qualifier = Q (PackagePath stage namespace qualifier) name
      case pkgSpecifierConstraints (SpecificSourcePackage (PackageIdentifier name version)) of
        [LabeledPackageConstraint constraint source] -> do
          constraint @?= PackageConstraint scope (PackagePropertyVersion (thisVersion version))
          source @?= ConstraintSourceUserTarget
        _ -> assertFailure "expected one version constraint for the local source target"
      constraintScopeMatches scope (goal Host DefaultNamespace QualToplevel) @?= True
      constraintScopeMatches scope (goal Host (Independent name) QualToplevel) @?= True
      constraintScopeMatches scope (goal Host (Independent other) QualToplevel) @?= False
      constraintScopeMatches scope (goal Build (Independent name) QualToplevel) @?= False
      constraintScopeMatches scope (goal Host (Independent name) (QualSetup name)) @?= False
      constraintScopeMatches (ConstraintScope (Just Host) (ScopeQualified QualToplevel name)) (goal Host (Independent other) QualToplevel) @?= True
  , makeGroup
      "readUserConstraint"
      (uncurry readUserConstraintTest)
      exampleConstraints
  , makeGroup
      "parseUserConstraint"
      (uncurry parseUserConstraintTest)
      exampleConstraints
  , makeGroup
      "print/parseUserConstraint"
      (\(_, constraint) -> parseUserConstraintTest (prettyShow constraint) constraint)
      exampleConstraints
  , makeGroup
      "readUserConstraints"
      (uncurry readUserConstraintsTest)
      [ -- First example only.

        ( case exampleStrs of (e : _) -> e; _ -> error "empty examples"
        , take 1 exampleUcs
        )
      , -- All examples separated by commas.
        (intercalate ", " exampleStrs, exampleUcs)
      ]
  ]
  where
    (exampleStrs, exampleUcs) = unzip exampleConstraints

exampleConstraints :: [(String, UserConstraint)]
exampleConstraints =
  [
    ( "template-haskell installed"
    , UserConstraint
        (UserQualified UserQualToplevel (pn "template-haskell"))
        PackagePropertyInstalled
    )
  ,
    ( "bytestring >= 0"
    , UserConstraint
        (UserQualified UserQualToplevel (pn "bytestring"))
        (PackagePropertyVersion anyVersion)
    )
  ,
    ( "any.directory test"
    , UserConstraint
        (UserAnyQualifier (pn "directory"))
        (PackagePropertyStanzas [TestStanzas])
    )
  ,
    ( "setup.Cabal installed"
    , UserConstraint
        (UserAnySetupQualifier (pn "Cabal"))
        PackagePropertyInstalled
    )
  ,
    ( "process:setup.bytestring ==5.2"
    , UserConstraint
        (UserQualified (UserQualSetup (pn "process")) (pn "bytestring"))
        (PackagePropertyVersion (thisVersion (mkVersion [5, 2])))
    )
  , -- flag MUST be prefixed with - or +

    ( "network:setup.containers +foo -bar +baz"
    , UserConstraint
        (UserQualified (UserQualSetup (pn "network")) (pn "containers"))
        ( PackagePropertyFlags
            ( mkFlagAssignment
                [ (fn "foo", True)
                , (fn "bar", False)
                , (fn "baz", True)
                ]
            )
        )
    )
    -- -- TODO: Re-enable UserQualExe tests once we decide on a syntax.
    --
    -- , ("foo:happy:exe.template-haskell test",
    --    UserConstraint (UserQualified (UserQualExe (pn "foo") (pn "happy")) (pn "template-haskell"))
    --                   (PackagePropertyStanzas [TestStanzas]))
  ]
    ++ stagedConstraints
  where
    pn = mkPackageName
    fn = mkFlagName

    stagedConstraints =
      [ ( showStage stage ++ ":" ++ scope ++ " source"
        , UserConstraintStaged stage qualifier PackagePropertySource
        )
      | stage <- [Build, Host]
      , (scope, qualifier) <-
          [ ("bytestring", UserQualified UserQualToplevel (pn "bytestring"))
          , ("any.bytestring", UserAnyQualifier (pn "bytestring"))
          , ("setup.Cabal", UserAnySetupQualifier (pn "Cabal"))
          , ("exe.alex", UserAnyExeQualifier (pn "alex"))
          , ("process:setup.bytestring", UserQualified (UserQualSetup (pn "process")) (pn "bytestring"))
          ]
      ]

readUserConstraintTest :: String -> UserConstraint -> Assertion
readUserConstraintTest str uc =
  assertEqual ("Couldn't read constraint: '" ++ str ++ "'") expected actual
  where
    expected = Right uc
    actual = readUserConstraint str

parseUserConstraintTest :: String -> UserConstraint -> Assertion
parseUserConstraintTest str uc =
  assertEqual ("Couldn't parse constraint: '" ++ str ++ "'") expected actual
  where
    expected = Right uc
    actual = explicitEitherParsec parsec str

readUserConstraintsTest :: String -> [UserConstraint] -> Assertion
readUserConstraintsTest str ucs =
  assertEqual ("Couldn't read constraints: '" ++ str ++ "'") expected actual
  where
    expected = Right ucs
    actual = explicitEitherParsec (parsecCommaList parsec) str
