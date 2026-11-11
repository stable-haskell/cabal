{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | 'GenericPackageDescription' Field descriptions
module Distribution.PackageDescription.FieldGrammar
  ( -- * Package description
    packageDescriptionFieldGrammar
  , CompatDataDir (..)
  , CompatLicenseFile (..)

    -- * Library
  , libraryFieldGrammar

    -- * Foreign library
  , foreignLibFieldGrammar

    -- * Executable
  , executableFieldGrammar

    -- * Test suite
  , TestSuiteStanza (..)
  , testSuiteFieldGrammar
  , validateTestSuite
  , unvalidateTestSuite

    -- ** Lenses
  , testStanzaTestType
  , testStanzaMainIs
  , testStanzaTestModule
  , testStanzaBuildInfo

    -- * Benchmark
  , BenchmarkStanza (..)
  , benchmarkFieldGrammar
  , validateBenchmark
  , unvalidateBenchmark

    -- * Field grammars
  , formatDependencyList
  , formatExposedModules
  , formatExtraSourceFiles
  , formatHsSourceDirs
  , formatMixinList
  , formatOtherExtensions
  , formatOtherModules

    -- ** Lenses
  , benchmarkStanzaBenchmarkType
  , benchmarkStanzaMainIs
  , benchmarkStanzaBenchmarkModule
  , benchmarkStanzaBuildInfo

    -- * Flag
  , flagFieldGrammar

    -- * Source repository
  , sourceRepoFieldGrammar

    -- * Setup build info
  , setupBInfoFieldGrammar

    -- * Component build info
  , buildInfoFieldGrammar
  ) where

import Distribution.Compat.Lens
import Distribution.Compat.Prelude
import Language.Haskell.Extension
import Prelude ()

import Distribution.CabalSpecVersion
import Distribution.Compiler (CompilerFlavor (..), PerCompilerFlavor (..))
import Distribution.FieldGrammar
import Distribution.Fields
import Distribution.ModuleName (ModuleName)
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Parsec
import Distribution.Pretty (Pretty (..), prettyShow, showToken)
import Distribution.Utils.Path
import Distribution.Version (Version, VersionRange)

import Data.Bifunctor
import qualified Data.ByteString.Char8 as BS8
import Data.Coerce (coerce)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Distribution.Compat.CharParsing as P
import qualified Distribution.SPDX as SPDX
import qualified Distribution.Types.Lens as L
import Distribution.Utils.Generic (ordNub)

-------------------------------------------------------------------------------
-- PackageDescription
-------------------------------------------------------------------------------

packageDescriptionFieldGrammar
  :: ( FieldGrammar c g
     , c (Identity BuildType)
     , c (Identity PackageName)
     , c (Identity Version)
     , c (List FSep (RelativePathNT Pkg File) (SymbolicPathX OnlyRelative Pkg File))
     , c (List VCat (RelativePathNT DataDir File) (RelativePath DataDir File))
     , c (List VCat (RelativePathNT Pkg File) (RelativePath Pkg File))
     , c (List FSep TestedWith (CompilerFlavor, VersionRange))
     , c CompatLicenseFile
     , c CompatDataDir
     )
  => g PackageDescription PackageDescription
#ifdef __MHS__
-- MicroHs does not implement ApplicativeDo, so the do-block below is
-- spelled out with explicit Applicative combinators. Semantics are
-- identical to the ApplicativeDo desugaring GHC performs.
packageDescriptionFieldGrammar =
  mkPackageDescription
    <$> optionalFieldDefAla "cabal-version" SpecVersion L.specVersion CabalSpecV1_0
    <*> blurFieldGrammar L.package packageIdentifierGrammar
    <*> optionalFieldDefAla "license" SpecLicense L.licenseRaw (Left SPDX.NONE)
    <*> licenseFilesGrammar
    <*> freeTextFieldDef "copyright" L.copyright
    <*> freeTextFieldDef "maintainer" L.maintainer
    <*> freeTextFieldDef "author" L.author
    <*> freeTextFieldDef "stability" L.stability
    <*> monoidalFieldAla "tested-with" (alaList' FSep TestedWith) L.testedWith
    <*> freeTextFieldDef "homepage" L.homepage
    <*> freeTextFieldDef "package-url" L.pkgUrl
    <*> freeTextFieldDef "bug-reports" L.bugReports
    <*> freeTextFieldDef "synopsis" L.synopsis
    <*> freeTextFieldDef "description" L.description
    <*> freeTextFieldDef "category" L.category
    <*> (map (bimap T.unpack T.unpack) <$> prefixedFields "x-" L.customFieldsPDText)
    <*> optionalField "build-type" L.buildTypeRaw
    <*> monoidalFieldAla "data-files" (alaList' VCat RelativePathNT) L.dataFiles
    <*> ( optionalFieldDefAla "data-dir" CompatDataDir L.dataDir sameDirectory
            ^^^ fmap (\x -> if null (getSymbolicPath x) then sameDirectory else x) -- map empty directories to "."
        )
    <*> monoidalFieldAla "extra-source-files" formatExtraSourceFiles L.extraSrcFiles
    <*> monoidalFieldAla "extra-tmp-files" (alaList' VCat RelativePathNT) L.extraTmpFiles
    <*> monoidalFieldAla "extra-doc-files" formatExtraSourceFiles L.extraDocFiles
    <*> ( monoidalFieldAla "extra-files" formatExtraSourceFiles L.extraFiles
            ^^^ availableSince CabalSpecV3_14 []
        )
  where
    mkPackageDescription specVersion package licenseRaw licenseFiles copyright maintainer author stability testedWith homepage pkgUrl bugReports synopsis description category customFieldsPD buildTypeRaw dataFiles dataDir extraSrcFiles extraTmpFiles extraDocFiles extraFiles =
      PackageDescription{..}
      where
        sourceRepos = []
        setupBuildInfo = Nothing
        -- components
        library = Nothing
        subLibraries = []
        executables = []
        foreignLibs = []
        testSuites = []
        benchmarks = []

    packageIdentifierGrammar =
      PackageIdentifier
        <$> uniqueField "name" L.pkgName
        <*> uniqueField "version" L.pkgVersion

    licenseFilesGrammar =
      (++)
        -- TODO: neither field is deprecated
        -- should we pretty print license-file if there's single license file
        -- and license-files when more
        <$> monoidalFieldAla "license-file" CompatLicenseFile L.licenseFiles
        <*> monoidalFieldAla "license-files" (alaList' FSep RelativePathNT) L.licenseFiles
          ^^^ hiddenField
#else
packageDescriptionFieldGrammar = do
  specVersion <- optionalFieldDefAla "cabal-version" SpecVersion L.specVersion CabalSpecV1_0
  package <- blurFieldGrammar L.package packageIdentifierGrammar
  licenseRaw <- optionalFieldDefAla "license" SpecLicense L.licenseRaw (Left SPDX.NONE)
  licenseFiles <- licenseFilesGrammar
  copyright <- freeTextFieldDef "copyright" L.copyright
  maintainer <- freeTextFieldDef "maintainer" L.maintainer
  author <- freeTextFieldDef "author" L.author
  stability <- freeTextFieldDef "stability" L.stability
  testedWith <- monoidalFieldAla "tested-with" (alaList' FSep TestedWith) L.testedWith
  homepage <- freeTextFieldDef "homepage" L.homepage
  pkgUrl <- freeTextFieldDef "package-url" L.pkgUrl
  bugReports <- freeTextFieldDef "bug-reports" L.bugReports
  let sourceRepos = []
  synopsis <- freeTextFieldDef "synopsis" L.synopsis
  description <- freeTextFieldDef "description" L.description
  category <- freeTextFieldDef "category" L.category
  customFieldsPD <- map (bimap T.unpack T.unpack) <$> prefixedFields "x-" L.customFieldsPDText
  buildTypeRaw <- optionalField "build-type" L.buildTypeRaw
  let setupBuildInfo = Nothing
      -- components
      library = Nothing
      subLibraries = []
      executables = []
      foreignLibs = []
      testSuites = []
      benchmarks = []
  --  * Files
  dataFiles <- monoidalFieldAla "data-files" (alaList' VCat RelativePathNT) L.dataFiles
  dataDir <-
    optionalFieldDefAla "data-dir" CompatDataDir L.dataDir sameDirectory
      ^^^ fmap (\x -> if null (getSymbolicPath x) then sameDirectory else x) -- map empty directories to "."
  extraSrcFiles <- monoidalFieldAla "extra-source-files" formatExtraSourceFiles L.extraSrcFiles
  extraTmpFiles <- monoidalFieldAla "extra-tmp-files" (alaList' VCat RelativePathNT) L.extraTmpFiles
  extraDocFiles <- monoidalFieldAla "extra-doc-files" formatExtraSourceFiles L.extraDocFiles
  extraFiles <-
    monoidalFieldAla "extra-files" formatExtraSourceFiles L.extraFiles
      ^^^ availableSince CabalSpecV3_14 []
  pure PackageDescription{..}
  where
    packageIdentifierGrammar = do
      pkgName <- uniqueField "name" L.pkgName
      pkgVersion <- uniqueField "version" L.pkgVersion
      pure PackageIdentifier{..}

    licenseFilesGrammar =
      (++)
        -- TODO: neither field is deprecated
        -- should we pretty print license-file if there's single license file
        -- and license-files when more
        <$> monoidalFieldAla "license-file" CompatLicenseFile L.licenseFiles
        <*> monoidalFieldAla "license-files" (alaList' FSep RelativePathNT) L.licenseFiles
          ^^^ hiddenField
#endif

-------------------------------------------------------------------------------
-- Library
-------------------------------------------------------------------------------

-- MicroHs supports tuples only up to 15 elements, so constraints beyond
-- that are nested into smaller tuples. Both GHC and MicroHs accept
-- nested constraint tuples with identical meaning.
libraryFieldGrammar
  :: ( ( FieldGrammar c g
       , c (Identity LibraryVisibility)
       , c (List CommaFSep (Identity ExeDependency) ExeDependency)
       , c (List CommaFSep (Identity LegacyExeDependency) LegacyExeDependency)
       , c (List CommaFSep (Identity PkgconfigDependency) PkgconfigDependency)
       , c (List CommaVCat (Identity Dependency) Dependency)
       , c (List CommaVCat (Identity Mixin) Mixin)
       , c (List CommaVCat (Identity ModuleReexport) ModuleReexport)
       , c (List FSep (MQuoted Extension) Extension)
       , c (List FSep (MQuoted Language) Language)
       , c (List NoCommaFSep Token' Text)
       )
     , ( c (List VCat (MQuoted ModuleName) ModuleName)
       , c (List FSep (SymbolicPathNT Pkg (Dir Framework)) (SymbolicPath Pkg (Dir Framework)))
       , c (List FSep (SymbolicPathNT Pkg (Dir Lib)) (SymbolicPath Pkg (Dir Lib)))
       , c (List FSep (SymbolicPathNT Pkg (Dir Source)) (SymbolicPath Pkg (Dir Source)))
       , c (List FSep (SymbolicPathNT Pkg (Dir Include)) (SymbolicPath Pkg (Dir Include)))
       , c (List FSep (SymbolicPathNT Include File) (SymbolicPath Include File))
       , c (List FSep (RelativePathNT Framework File) (RelativePath Framework File))
       , c (List FSep (RelativePathNT Include File) (RelativePath Include File))
       , c (List VCat (SymbolicPathNT Pkg File) (SymbolicPath Pkg File))
       , c (List VCat Token Text)
       , c (MQuoted Language)
       )
     )
  => LibraryName
  -> g Library Library
#ifdef __MHS__
-- MicroHs does not implement ApplicativeDo, so the do-block below is
-- spelled out with explicit Applicative combinators. Semantics are
-- identical to the ApplicativeDo desugaring GHC performs.
libraryFieldGrammar libName =
  mkLibrary
    <$> monoidalFieldAla "exposed-modules" formatExposedModules L.exposedModules
    <*> monoidalFieldAla "reexported-modules" (alaList CommaVCat) L.reexportedModules
    <*> ( monoidalFieldAla "signatures" (alaList' VCat MQuoted) L.signatures
            ^^^ availableSince CabalSpecV2_0 []
        )
    <*> booleanFieldDef "exposed" L.libExposed True
    <*> visibilityField
    <*> blurFieldGrammar L.libBuildInfo buildInfoFieldGrammar
  where
    mkLibrary exposedModules reexportedModules signatures libExposed libVisibility libBuildInfo =
      Library{..}

    visibilityField = case libName of
      -- nameless/"main" libraries are public
      LMainLibName -> pure LibraryVisibilityPublic
      -- named libraries have the field
      LSubLibName _ ->
        optionalFieldDef "visibility" L.libVisibility LibraryVisibilityPrivate
          ^^^ availableSince CabalSpecV3_0 LibraryVisibilityPrivate
#else
libraryFieldGrammar libName = do
  exposedModules <- monoidalFieldAla "exposed-modules" formatExposedModules L.exposedModules
  reexportedModules <- monoidalFieldAla "reexported-modules" (alaList CommaVCat) L.reexportedModules
  signatures <-
    monoidalFieldAla "signatures" (alaList' VCat MQuoted) L.signatures
      ^^^ availableSince CabalSpecV2_0 []
  libExposed <- booleanFieldDef "exposed" L.libExposed True
  libVisibility <- visibilityField
  libBuildInfo <- blurFieldGrammar L.libBuildInfo buildInfoFieldGrammar
  pure Library{..}
  where
    visibilityField = case libName of
      -- nameless/"main" libraries are public
      LMainLibName -> pure LibraryVisibilityPublic
      -- named libraries have the field
      LSubLibName _ ->
        optionalFieldDef "visibility" L.libVisibility LibraryVisibilityPrivate
          ^^^ availableSince CabalSpecV3_0 LibraryVisibilityPrivate
#endif
{-# SPECIALIZE libraryFieldGrammar :: LibraryName -> ParsecFieldGrammar' Library #-}
{-# SPECIALIZE libraryFieldGrammar :: LibraryName -> PrettyFieldGrammar' Library #-}

-------------------------------------------------------------------------------
-- Foreign library
-------------------------------------------------------------------------------

foreignLibFieldGrammar
  :: ( ( FieldGrammar c g
       , c (Identity ForeignLibType)
       , c (Identity LibVersionInfo)
       , c (Identity Version)
       , c (List CommaFSep (Identity ExeDependency) ExeDependency)
       , c (List CommaFSep (Identity LegacyExeDependency) LegacyExeDependency)
       , c (List CommaFSep (Identity PkgconfigDependency) PkgconfigDependency)
       , c (List CommaVCat (Identity Dependency) Dependency)
       , c (List CommaVCat (Identity Mixin) Mixin)
       , c (List FSep (Identity ForeignLibOption) ForeignLibOption)
       , c (List FSep (MQuoted Extension) Extension)
       , c (List FSep (MQuoted Language) Language)
       , c (List FSep (SymbolicPathNT Pkg (Dir Framework)) (SymbolicPath Pkg (Dir Framework)))
       )
     , ( c (List FSep (SymbolicPathNT Pkg (Dir Lib)) (SymbolicPath Pkg (Dir Lib)))
       , c (List FSep (SymbolicPathNT Pkg (Dir Source)) (SymbolicPath Pkg (Dir Source)))
       , c (List FSep (SymbolicPathNT Pkg (Dir Include)) (SymbolicPath Pkg (Dir Include)))
       , c (List FSep (SymbolicPathNT Include File) (SymbolicPath Include File))
       , c (List FSep (RelativePathNT Framework File) (RelativePath Framework File))
       , c (List FSep (RelativePathNT Include File) (RelativePath Include File))
       , c (List FSep (RelativePathNT Source File) (RelativePath Source File))
       , c (List VCat (SymbolicPathNT Pkg File) (SymbolicPath Pkg File))
       , c (List NoCommaFSep Token' Text)
       , c (List VCat (MQuoted ModuleName) ModuleName)
       , c (List VCat Token Text)
       , c (MQuoted Language)
       )
     )
  => UnqualComponentName
  -> g ForeignLib ForeignLib
#ifdef __MHS__
-- MicroHs does not implement ApplicativeDo, so the do-block below is
-- spelled out with explicit Applicative combinators. Semantics are
-- identical to the ApplicativeDo desugaring GHC performs.
foreignLibFieldGrammar foreignLibName =
  ForeignLib foreignLibName
    <$> optionalFieldDef "type" L.foreignLibType ForeignLibTypeUnknown
    <*> monoidalFieldAla "options" (alaList FSep) L.foreignLibOptions
    <*> blurFieldGrammar L.foreignLibBuildInfo buildInfoFieldGrammar
    <*> optionalField "lib-version-info" L.foreignLibVersionInfo
    <*> optionalField "lib-version-linux" L.foreignLibVersionLinux
    <*> monoidalFieldAla "mod-def-file" (alaList' FSep RelativePathNT) L.foreignLibModDefFile
#else
foreignLibFieldGrammar foreignLibName = do
  foreignLibType <- optionalFieldDef "type" L.foreignLibType ForeignLibTypeUnknown
  foreignLibOptions <- monoidalFieldAla "options" (alaList FSep) L.foreignLibOptions
  foreignLibBuildInfo <- blurFieldGrammar L.foreignLibBuildInfo buildInfoFieldGrammar
  foreignLibVersionInfo <- optionalField "lib-version-info" L.foreignLibVersionInfo
  foreignLibVersionLinux <- optionalField "lib-version-linux" L.foreignLibVersionLinux
  foreignLibModDefFile <- monoidalFieldAla "mod-def-file" (alaList' FSep RelativePathNT) L.foreignLibModDefFile
  pure ForeignLib{..}
#endif
{-# SPECIALIZE foreignLibFieldGrammar :: UnqualComponentName -> ParsecFieldGrammar' ForeignLib #-}
{-# SPECIALIZE foreignLibFieldGrammar :: UnqualComponentName -> PrettyFieldGrammar' ForeignLib #-}

-------------------------------------------------------------------------------
-- Executable
-------------------------------------------------------------------------------

executableFieldGrammar
  :: ( ( FieldGrammar c g
       , c (Identity ExecutableScope)
       , c (List CommaFSep (Identity ExeDependency) ExeDependency)
       , c (List CommaFSep (Identity LegacyExeDependency) LegacyExeDependency)
       , c (List CommaFSep (Identity PkgconfigDependency) PkgconfigDependency)
       , c (List CommaVCat (Identity Dependency) Dependency)
       , c (List CommaVCat (Identity Mixin) Mixin)
       , c (List FSep (MQuoted Extension) Extension)
       , c (List FSep (MQuoted Language) Language)
       , c (List FSep (SymbolicPathNT Pkg (Dir Framework)) (SymbolicPath Pkg (Dir Framework)))
       , c (List FSep (SymbolicPathNT Pkg (Dir Lib)) (SymbolicPath Pkg (Dir Lib)))
       )
     , ( c (List FSep (SymbolicPathNT Pkg (Dir Source)) (SymbolicPath Pkg (Dir Source)))
       , c (List FSep (SymbolicPathNT Pkg (Dir Include)) (SymbolicPath Pkg (Dir Include)))
       , c (List FSep (SymbolicPathNT Include File) (SymbolicPath Include File))
       , c (List FSep (RelativePathNT Framework File) (RelativePath Framework File))
       , c (List FSep (RelativePathNT Include File) (RelativePath Include File))
       , c (List VCat (SymbolicPathNT Pkg File) (SymbolicPath Pkg File))
       , c (RelativePathNT Source File)
       , c (List NoCommaFSep Token' Text)
       , c (List VCat (MQuoted ModuleName) ModuleName)
       , c (List VCat Token Text)
       , c (MQuoted Language)
       )
     )
  => UnqualComponentName
  -> g Executable Executable
#ifdef __MHS__
-- MicroHs does not implement ApplicativeDo, so the do-block below is
-- spelled out with explicit Applicative combinators. Semantics are
-- identical to the ApplicativeDo desugaring GHC performs.
executableFieldGrammar exeName =
  Executable exeName
    -- main-is is optional as conditional blocks don't have it
    <$> optionalFieldDefAla "main-is" RelativePathNT L.modulePath (modulePath mempty)
    <*> ( optionalFieldDef "scope" L.exeScope ExecutablePublic
            ^^^ availableSince CabalSpecV2_0 ExecutablePublic
        )
    <*> blurFieldGrammar L.buildInfo buildInfoFieldGrammar
#else
executableFieldGrammar exeName = do
  -- main-is is optional as conditional blocks don't have it
  modulePath <- optionalFieldDefAla "main-is" RelativePathNT L.modulePath (modulePath mempty)
  exeScope <-
    optionalFieldDef "scope" L.exeScope ExecutablePublic
      ^^^ availableSince CabalSpecV2_0 ExecutablePublic
  buildInfo <- blurFieldGrammar L.buildInfo buildInfoFieldGrammar
  pure Executable{..}
#endif
{-# SPECIALIZE executableFieldGrammar :: UnqualComponentName -> ParsecFieldGrammar' Executable #-}
{-# SPECIALIZE executableFieldGrammar :: UnqualComponentName -> PrettyFieldGrammar' Executable #-}

-------------------------------------------------------------------------------
-- TestSuite
-------------------------------------------------------------------------------

-- | An intermediate type just used for parsing the test-suite stanza.
-- After validation it is converted into the proper 'TestSuite' type.
data TestSuiteStanza = TestSuiteStanza
  { _testStanzaTestType :: Maybe TestType
  , _testStanzaMainIs :: Maybe (RelativePath Source File)
  , _testStanzaTestModule :: Maybe ModuleName
  , _testStanzaBuildInfo :: BuildInfo
  , _testStanzaCodeGenerators :: [Text]
  }

instance L.HasBuildInfo TestSuiteStanza where
  buildInfo = testStanzaBuildInfo

testStanzaTestType :: Lens' TestSuiteStanza (Maybe TestType)
testStanzaTestType f s = fmap (\x -> s{_testStanzaTestType = x}) (f (_testStanzaTestType s))
{-# INLINE testStanzaTestType #-}

testStanzaMainIs :: Lens' TestSuiteStanza (Maybe (RelativePath Source File))
testStanzaMainIs f s = fmap (\x -> s{_testStanzaMainIs = x}) (f (_testStanzaMainIs s))
{-# INLINE testStanzaMainIs #-}

testStanzaTestModule :: Lens' TestSuiteStanza (Maybe ModuleName)
testStanzaTestModule f s = fmap (\x -> s{_testStanzaTestModule = x}) (f (_testStanzaTestModule s))
{-# INLINE testStanzaTestModule #-}

testStanzaBuildInfo :: Lens' TestSuiteStanza BuildInfo
testStanzaBuildInfo f s = fmap (\x -> s{_testStanzaBuildInfo = x}) (f (_testStanzaBuildInfo s))
{-# INLINE testStanzaBuildInfo #-}

testStanzaCodeGenerators :: Lens' TestSuiteStanza [Text]
testStanzaCodeGenerators f s = fmap (\x -> s{_testStanzaCodeGenerators = x}) (f (_testStanzaCodeGenerators s))
{-# INLINE testStanzaCodeGenerators #-}

testSuiteFieldGrammar
  :: ( ( FieldGrammar c g
       , c (Identity ModuleName)
       , c (Identity TestType)
       , c (List CommaFSep (Identity ExeDependency) ExeDependency)
       , c (List CommaFSep (Identity LegacyExeDependency) LegacyExeDependency)
       , c (List CommaFSep (Identity PkgconfigDependency) PkgconfigDependency)
       , c (List CommaFSep Token Text)
       , c (List CommaVCat (Identity Dependency) Dependency)
       , c (List CommaVCat (Identity Mixin) Mixin)
       , c (List FSep (MQuoted Extension) Extension)
       , c (List FSep (MQuoted Language) Language)
       , c (List NoCommaFSep Token' Text)
       )
     , ( c (List VCat (MQuoted ModuleName) ModuleName)
       , c (List FSep (SymbolicPathNT Pkg (Dir Framework)) (SymbolicPath Pkg (Dir Framework)))
       , c (List FSep (SymbolicPathNT Pkg (Dir Lib)) (SymbolicPath Pkg (Dir Lib)))
       , c (List FSep (SymbolicPathNT Pkg (Dir Source)) (SymbolicPath Pkg (Dir Source)))
       , c (List FSep (SymbolicPathNT Pkg (Dir Include)) (SymbolicPath Pkg (Dir Include)))
       , c (List FSep (SymbolicPathNT Include File) (SymbolicPath Include File))
       , c (List FSep (RelativePathNT Framework File) (RelativePath Framework File))
       , c (List FSep (RelativePathNT Include File) (RelativePath Include File))
       , c (List VCat (SymbolicPathNT Pkg File) (SymbolicPath Pkg File))
       , c (RelativePathNT Source File)
       , c (List VCat Token Text)
       , c (MQuoted Language)
       )
     )
  => g TestSuiteStanza TestSuiteStanza
#ifdef __MHS__
-- MicroHs does not implement ApplicativeDo, so the do-block below is
-- spelled out with explicit Applicative combinators. Semantics are
-- identical to the ApplicativeDo desugaring GHC performs.
testSuiteFieldGrammar =
  TestSuiteStanza
    <$> optionalField "type" testStanzaTestType
    <*> optionalFieldAla "main-is" RelativePathNT testStanzaMainIs
    <*> optionalField "test-module" testStanzaTestModule
    <*> blurFieldGrammar testStanzaBuildInfo buildInfoFieldGrammar
    <*> ( monoidalFieldAla "code-generators" (alaList' CommaFSep Token) testStanzaCodeGenerators
            ^^^ availableSince CabalSpecV3_8 []
        )
#else
testSuiteFieldGrammar = do
  _testStanzaTestType <- optionalField "type" testStanzaTestType
  _testStanzaMainIs <- optionalFieldAla "main-is" RelativePathNT testStanzaMainIs
  _testStanzaTestModule <- optionalField "test-module" testStanzaTestModule
  _testStanzaBuildInfo <- blurFieldGrammar testStanzaBuildInfo buildInfoFieldGrammar
  _testStanzaCodeGenerators <-
    monoidalFieldAla "code-generators" (alaList' CommaFSep Token) testStanzaCodeGenerators
      ^^^ availableSince CabalSpecV3_8 []
  pure TestSuiteStanza{..}
#endif

validateTestSuite :: CabalSpecVersion -> Position -> TestSuiteStanza -> ParseResult src TestSuite
validateTestSuite cabalSpecVersion pos stanza = case testSuiteType of
  Nothing -> pure basicTestSuite
  Just tt@(TestTypeUnknown _ _) ->
    pure
      basicTestSuite
        { testInterface = TestSuiteUnsupported tt
        }
  Just tt
    | tt `notElem` knownTestTypes ->
        pure
          basicTestSuite
            { testInterface = TestSuiteUnsupported tt
            }
  Just tt@(TestTypeExe ver) -> case _testStanzaMainIs stanza of
    Nothing -> do
      parseFailure pos (missingField "main-is" tt)
      pure emptyTestSuite
    Just file -> do
      when (isJust (_testStanzaTestModule stanza)) $
        parseWarning pos PWTExtraBenchmarkModule (extraField "test-module" tt)
      pure
        basicTestSuite
          { testInterface = TestSuiteExeV10 ver file
          }
  Just tt@(TestTypeLib ver) -> case _testStanzaTestModule stanza of
    Nothing -> do
      parseFailure pos (missingField "test-module" tt)
      pure emptyTestSuite
    Just module_ -> do
      when (isJust (_testStanzaMainIs stanza)) $
        parseWarning pos PWTExtraMainIs (extraField "main-is" tt)
      pure
        basicTestSuite
          { testInterface = TestSuiteLibV09 ver module_
          }
  where
    testSuiteType =
      _testStanzaTestType stanza
        <|> do
          guard (cabalSpecVersion >= CabalSpecV3_8)

          testTypeExe <$ _testStanzaMainIs stanza
        <|> testTypeLib <$ _testStanzaTestModule stanza

    missingField name tt =
      "The '"
        ++ name
        ++ "' field is required for the "
        ++ prettyShow tt
        ++ " test suite type."

    extraField name tt =
      "The '"
        ++ name
        ++ "' field is not used for the '"
        ++ prettyShow tt
        ++ "' test suite type."
    basicTestSuite =
      emptyTestSuite
        { testBuildInfo = _testStanzaBuildInfo stanza
        , testCodeGenerators = _testStanzaCodeGenerators stanza
        }

unvalidateTestSuite :: TestSuite -> TestSuiteStanza
unvalidateTestSuite t =
  TestSuiteStanza
    { _testStanzaTestType = ty
    , _testStanzaMainIs = ma
    , _testStanzaTestModule = mo
    , _testStanzaBuildInfo = testBuildInfo t
    , _testStanzaCodeGenerators = testCodeGenerators t
    }
  where
    (ty, ma, mo) = case testInterface t of
      TestSuiteExeV10 ver file -> (Just $ TestTypeExe ver, Just file, Nothing)
      TestSuiteLibV09 ver modu -> (Just $ TestTypeLib ver, Nothing, Just modu)
      _ -> (Nothing, Nothing, Nothing)

-------------------------------------------------------------------------------
-- Benchmark
-------------------------------------------------------------------------------

-- | An intermediate type just used for parsing the benchmark stanza.
-- After validation it is converted into the proper 'Benchmark' type.
data BenchmarkStanza = BenchmarkStanza
  { _benchmarkStanzaBenchmarkType :: Maybe BenchmarkType
  , _benchmarkStanzaMainIs :: Maybe (RelativePath Source File)
  , _benchmarkStanzaBenchmarkModule :: Maybe ModuleName
  , _benchmarkStanzaBuildInfo :: BuildInfo
  }

instance L.HasBuildInfo BenchmarkStanza where
  buildInfo = benchmarkStanzaBuildInfo

benchmarkStanzaBenchmarkType :: Lens' BenchmarkStanza (Maybe BenchmarkType)
benchmarkStanzaBenchmarkType f s = fmap (\x -> s{_benchmarkStanzaBenchmarkType = x}) (f (_benchmarkStanzaBenchmarkType s))
{-# INLINE benchmarkStanzaBenchmarkType #-}

benchmarkStanzaMainIs :: Lens' BenchmarkStanza (Maybe (RelativePath Source File))
benchmarkStanzaMainIs f s = fmap (\x -> s{_benchmarkStanzaMainIs = x}) (f (_benchmarkStanzaMainIs s))
{-# INLINE benchmarkStanzaMainIs #-}

benchmarkStanzaBenchmarkModule :: Lens' BenchmarkStanza (Maybe ModuleName)
benchmarkStanzaBenchmarkModule f s = fmap (\x -> s{_benchmarkStanzaBenchmarkModule = x}) (f (_benchmarkStanzaBenchmarkModule s))
{-# INLINE benchmarkStanzaBenchmarkModule #-}

benchmarkStanzaBuildInfo :: Lens' BenchmarkStanza BuildInfo
benchmarkStanzaBuildInfo f s = fmap (\x -> s{_benchmarkStanzaBuildInfo = x}) (f (_benchmarkStanzaBuildInfo s))
{-# INLINE benchmarkStanzaBuildInfo #-}

benchmarkFieldGrammar
  :: ( ( FieldGrammar c g
       , c (Identity BenchmarkType)
       , c (Identity ModuleName)
       , c (List CommaFSep (Identity ExeDependency) ExeDependency)
       , c (List CommaFSep (Identity LegacyExeDependency) LegacyExeDependency)
       , c (List CommaFSep (Identity PkgconfigDependency) PkgconfigDependency)
       , c (List CommaVCat (Identity Dependency) Dependency)
       , c (List CommaVCat (Identity Mixin) Mixin)
       , c (List FSep (MQuoted Extension) Extension)
       , c (List FSep (MQuoted Language) Language)
       , c (List NoCommaFSep Token' Text)
       , c (List VCat (MQuoted ModuleName) ModuleName)
       )
     , ( c (List FSep (SymbolicPathNT Pkg (Dir Framework)) (SymbolicPath Pkg (Dir Framework)))
       , c (List FSep (SymbolicPathNT Pkg (Dir Lib)) (SymbolicPath Pkg (Dir Lib)))
       , c (List FSep (SymbolicPathNT Pkg (Dir Source)) (SymbolicPath Pkg (Dir Source)))
       , c (List FSep (SymbolicPathNT Pkg (Dir Include)) (SymbolicPath Pkg (Dir Include)))
       , c (List FSep (SymbolicPathNT Include File) (SymbolicPath Include File))
       , c (List FSep (RelativePathNT Framework File) (RelativePath Framework File))
       , c (List FSep (RelativePathNT Include File) (RelativePath Include File))
       , c (List VCat (SymbolicPathNT Pkg File) (SymbolicPath Pkg File))
       , c (RelativePathNT Source File)
       , c (List VCat Token Text)
       , c (MQuoted Language)
       )
     )
  => g BenchmarkStanza BenchmarkStanza
#ifdef __MHS__
-- MicroHs does not implement ApplicativeDo, so the do-block below is
-- spelled out with explicit Applicative combinators. Semantics are
-- identical to the ApplicativeDo desugaring GHC performs.
benchmarkFieldGrammar =
  BenchmarkStanza
    <$> optionalField "type" benchmarkStanzaBenchmarkType
    <*> optionalFieldAla "main-is" RelativePathNT benchmarkStanzaMainIs
    <*> optionalField "benchmark-module" benchmarkStanzaBenchmarkModule
    <*> blurFieldGrammar benchmarkStanzaBuildInfo buildInfoFieldGrammar
#else
benchmarkFieldGrammar = do
  _benchmarkStanzaBenchmarkType <- optionalField "type" benchmarkStanzaBenchmarkType
  _benchmarkStanzaMainIs <- optionalFieldAla "main-is" RelativePathNT benchmarkStanzaMainIs
  _benchmarkStanzaBenchmarkModule <- optionalField "benchmark-module" benchmarkStanzaBenchmarkModule
  _benchmarkStanzaBuildInfo <- blurFieldGrammar benchmarkStanzaBuildInfo buildInfoFieldGrammar
  pure BenchmarkStanza{..}
#endif

validateBenchmark :: CabalSpecVersion -> Position -> BenchmarkStanza -> ParseResult src Benchmark
validateBenchmark cabalSpecVersion pos stanza = case benchmarkStanzaType of
  Nothing ->
    pure
      emptyBenchmark
        { benchmarkBuildInfo = _benchmarkStanzaBuildInfo stanza
        }
  Just tt@(BenchmarkTypeUnknown _ _) ->
    pure
      emptyBenchmark
        { benchmarkInterface = BenchmarkUnsupported tt
        , benchmarkBuildInfo = _benchmarkStanzaBuildInfo stanza
        }
  Just tt
    | tt `notElem` knownBenchmarkTypes ->
        pure
          emptyBenchmark
            { benchmarkInterface = BenchmarkUnsupported tt
            , benchmarkBuildInfo = _benchmarkStanzaBuildInfo stanza
            }
  Just tt@(BenchmarkTypeExe ver) -> case _benchmarkStanzaMainIs stanza of
    Nothing -> do
      parseFailure pos (missingField "main-is" tt)
      pure emptyBenchmark
    Just file -> do
      when (isJust (_benchmarkStanzaBenchmarkModule stanza)) $
        parseWarning pos PWTExtraBenchmarkModule (extraField "benchmark-module" tt)
      pure
        emptyBenchmark
          { benchmarkInterface = BenchmarkExeV10 ver file
          , benchmarkBuildInfo = _benchmarkStanzaBuildInfo stanza
          }
  where
    benchmarkStanzaType =
      _benchmarkStanzaBenchmarkType stanza <|> do
        guard (cabalSpecVersion >= CabalSpecV3_8)

        benchmarkTypeExe <$ _benchmarkStanzaMainIs stanza

    missingField name tt =
      "The '"
        ++ name
        ++ "' field is required for the "
        ++ prettyShow tt
        ++ " benchmark type."

    extraField name tt =
      "The '"
        ++ name
        ++ "' field is not used for the '"
        ++ prettyShow tt
        ++ "' benchmark type."

unvalidateBenchmark :: Benchmark -> BenchmarkStanza
unvalidateBenchmark b =
  BenchmarkStanza
    { _benchmarkStanzaBenchmarkType = ty
    , _benchmarkStanzaMainIs = ma
    , _benchmarkStanzaBenchmarkModule = mo
    , _benchmarkStanzaBuildInfo = benchmarkBuildInfo b
    }
  where
    (ty, ma, mo) = case benchmarkInterface b of
      BenchmarkExeV10 ver ma'
        | getSymbolicPath ma' == "" ->
            (Just $ BenchmarkTypeExe ver, Nothing, Nothing)
        | otherwise ->
            (Just $ BenchmarkTypeExe ver, Just ma', Nothing)
      _ -> (Nothing, Nothing, Nothing)

-------------------------------------------------------------------------------
-- Build info
-------------------------------------------------------------------------------

buildInfoFieldGrammar
  :: ( ( FieldGrammar c g
       , c (List CommaFSep (Identity ExeDependency) ExeDependency)
       , c (List CommaFSep (Identity LegacyExeDependency) LegacyExeDependency)
       , c (List CommaFSep (Identity PkgconfigDependency) PkgconfigDependency)
       , c (List CommaVCat (Identity Dependency) Dependency)
       , c (List CommaVCat (Identity Mixin) Mixin)
       , c (List FSep (MQuoted Extension) Extension)
       , c (List FSep (MQuoted Language) Language)
       , c (List NoCommaFSep Token' Text)
       , c (List VCat (MQuoted ModuleName) ModuleName)
       )
     , ( c (List FSep (SymbolicPathNT Pkg (Dir Framework)) (SymbolicPath Pkg (Dir Framework)))
       , c (List FSep (SymbolicPathNT Pkg (Dir Lib)) (SymbolicPath Pkg (Dir Lib)))
       , c (List FSep (SymbolicPathNT Pkg (Dir Source)) (SymbolicPath Pkg (Dir Source)))
       , c (List FSep (SymbolicPathNT Pkg (Dir Include)) (SymbolicPath Pkg (Dir Include)))
       , c (List FSep (SymbolicPathNT Include File) (SymbolicPath Include File))
       , c (List FSep (RelativePathNT Framework File) (RelativePath Framework File))
       , c (List FSep (RelativePathNT Include File) (RelativePath Include File))
       , c (List VCat (SymbolicPathNT Pkg File) (SymbolicPath Pkg File))
       , c (List VCat Token Text)
       , c (MQuoted Language)
       )
     )
  => g BuildInfo BuildInfo
#ifdef __MHS__
-- MicroHs does not implement ApplicativeDo, so the do-block below is
-- spelled out with explicit Applicative combinators. Semantics are
-- identical to the ApplicativeDo desugaring GHC performs.
buildInfoFieldGrammar =
  mkBuildInfo
    <$> booleanFieldDef "buildable" L.buildable True
    <*> ( monoidalFieldAla "build-tools" (alaList CommaFSep) L.buildTools
            ^^^ deprecatedSince
              CabalSpecV2_0
              "Please use 'build-tool-depends' field"
            ^^^ removedIn
              CabalSpecV3_0
              "Please use 'build-tool-depends' field."
        )
    -- {- ^^^ availableSince [2,0] [] -}
    -- here, we explicitly want to recognise build-tool-depends for all Cabal files
    -- as otherwise cabal new-build cannot really work.
    --
    -- I.e. we don't want trigger unknown field warning
    <*> monoidalFieldAla "build-tool-depends" (alaList CommaFSep) L.buildToolDepends
    <*> monoidalFieldAla "cpp-options" (alaList' NoCommaFSep Token') L.cppOptions
    <*> ( monoidalFieldAla "asm-options" (alaList' NoCommaFSep Token') L.asmOptions
            ^^^ availableSince CabalSpecV3_0 []
        )
    <*> ( monoidalFieldAla "cmm-options" (alaList' NoCommaFSep Token') L.cmmOptions
            ^^^ availableSince CabalSpecV3_0 []
        )
    <*> monoidalFieldAla "cc-options" (alaList' NoCommaFSep Token') L.ccOptions
    <*> ( monoidalFieldAla "cxx-options" (alaList' NoCommaFSep Token') L.cxxOptions
            ^^^ availableSince CabalSpecV2_2 []
        )
    <*> ( monoidalFieldAla "jspp-options" (alaList' NoCommaFSep Token') L.jsppOptions
            ^^^ availableSince CabalSpecV3_16 []
        )
    <*> monoidalFieldAla "ld-options" (alaList' NoCommaFSep Token') L.ldOptions
    <*> ( monoidalFieldAla "hsc2hs-options" (alaList' NoCommaFSep Token') L.hsc2hsOptions
            ^^^ availableSince CabalSpecV3_6 []
        )
    <*> monoidalFieldAla "pkgconfig-depends" (alaList CommaFSep) L.pkgconfigDepends
    <*> monoidalFieldAla "frameworks" (alaList' FSep RelativePathNT) L.frameworks
    <*> monoidalFieldAla "extra-framework-dirs" (alaList' FSep SymbolicPathNT) L.extraFrameworkDirs
    <*> ( monoidalFieldAla "asm-sources" (alaList' VCat SymbolicPathNT) L.asmSources
            ^^^ availableSince CabalSpecV3_0 []
        )
    <*> ( monoidalFieldAla "cmm-sources" (alaList' VCat SymbolicPathNT) L.cmmSources
            ^^^ availableSince CabalSpecV3_0 []
        )
    <*> monoidalFieldAla "c-sources" (alaList' VCat SymbolicPathNT) L.cSources
    <*> ( monoidalFieldAla "cxx-sources" (alaList' VCat SymbolicPathNT) L.cxxSources
            ^^^ availableSince CabalSpecV2_2 []
        )
    <*> monoidalFieldAla "js-sources" (alaList' VCat SymbolicPathNT) L.jsSources
    <*> hsSourceDirsGrammar
    <*> monoidalFieldAla "other-modules" formatOtherModules L.otherModules
    <*> ( monoidalFieldAla "virtual-modules" (alaList' VCat MQuoted) L.virtualModules
            ^^^ availableSince CabalSpecV2_2 []
        )
    <*> ( monoidalFieldAla "autogen-modules" (alaList' VCat MQuoted) L.autogenModules
            ^^^ availableSince CabalSpecV2_0 []
        )
    <*> ( optionalFieldAla "default-language" MQuoted L.defaultLanguage
            ^^^ availableSince CabalSpecV1_10 Nothing
        )
    <*> ( monoidalFieldAla "other-languages" (alaList' FSep MQuoted) L.otherLanguages
            ^^^ availableSince CabalSpecV1_10 []
        )
    <*> ( monoidalFieldAla "default-extensions" (alaList' FSep MQuoted) L.defaultExtensions
            ^^^ availableSince CabalSpecV1_10 []
        )
    <*> ( monoidalFieldAla "other-extensions" formatOtherExtensions L.otherExtensions
            ^^^ availableSinceWarn CabalSpecV1_10
        )
    <*> ( monoidalFieldAla "extensions" (alaList' FSep MQuoted) L.oldExtensions
            ^^^ deprecatedSince
              CabalSpecV1_12
              "Please use 'default-extensions' or 'other-extensions' fields."
            ^^^ removedIn
              CabalSpecV3_0
              "Please use 'default-extensions' or 'other-extensions' fields."
        )
    <*> monoidalFieldAla "extra-libraries" (alaList' VCat Token) L.extraLibs
    <*> ( monoidalFieldAla "extra-libraries-static" (alaList' VCat Token) L.extraLibsStatic
            ^^^ availableSince CabalSpecV3_8 []
        )
    <*> monoidalFieldAla "extra-ghci-libraries" (alaList' VCat Token) L.extraGHCiLibs
    <*> monoidalFieldAla "extra-bundled-libraries" (alaList' VCat Token) L.extraBundledLibs
    <*> monoidalFieldAla "extra-library-flavours" (alaList' VCat Token) L.extraLibFlavours
    <*> ( monoidalFieldAla "extra-dynamic-library-flavours" (alaList' VCat Token) L.extraDynLibFlavours
            ^^^ availableSince CabalSpecV3_0 []
        )
    <*> monoidalFieldAla "extra-lib-dirs" (alaList' FSep SymbolicPathNT) L.extraLibDirs
    <*> ( monoidalFieldAla "extra-lib-dirs-static" (alaList' FSep SymbolicPathNT) L.extraLibDirsStatic
            ^^^ availableSince CabalSpecV3_8 []
        )
    <*> monoidalFieldAla "include-dirs" (alaList' FSep SymbolicPathNT) L.includeDirs
    <*> monoidalFieldAla "includes" (alaList' FSep SymbolicPathNT) L.includes
    <*> ( monoidalFieldAla "autogen-includes" (alaList' FSep RelativePathNT) L.autogenIncludes
            ^^^ availableSince CabalSpecV3_0 []
        )
    <*> monoidalFieldAla "install-includes" (alaList' FSep RelativePathNT) L.installIncludes
    <*> optionsFieldGrammar
    <*> profOptionsFieldGrammar
    <*> sharedOptionsFieldGrammar
    <*> profSharedOptionsFieldGrammar
    <*> prefixedFields "x-" L.customFieldsBI
    <*> monoidalFieldAla "build-depends" formatDependencyList L.targetBuildDepends
    <*> ( monoidalFieldAla "mixins" formatMixinList L.mixins
            ^^^ availableSince CabalSpecV2_0 []
        )
  where
    mkBuildInfo buildable buildTools buildToolDepends cppOptions asmOptions cmmOptions ccOptions cxxOptions jsppOptions ldOptions hsc2hsOptions pkgconfigDepends frameworks extraFrameworkDirs asmSources cmmSources cSources cxxSources jsSources hsSourceDirs otherModules virtualModules autogenModules defaultLanguage otherLanguages defaultExtensions otherExtensions oldExtensions extraLibs extraLibsStatic extraGHCiLibs extraBundledLibs extraLibFlavours extraDynLibFlavours extraLibDirs extraLibDirsStatic includeDirs includes autogenIncludes installIncludes options profOptions sharedOptions profSharedOptions customFieldsBI targetBuildDepends mixins =
      BuildInfo{..}
      where
        staticOptions = mempty
#else
buildInfoFieldGrammar = do
  buildable <- booleanFieldDef "buildable" L.buildable True
  buildTools <-
    monoidalFieldAla "build-tools" (alaList CommaFSep) L.buildTools
      ^^^ deprecatedSince
        CabalSpecV2_0
        "Please use 'build-tool-depends' field"
      ^^^ removedIn
        CabalSpecV3_0
        "Please use 'build-tool-depends' field."
  buildToolDepends <- monoidalFieldAla "build-tool-depends" (alaList CommaFSep) L.buildToolDepends
  -- {- ^^^ availableSince [2,0] [] -}
  -- here, we explicitly want to recognise build-tool-depends for all Cabal files
  -- as otherwise cabal new-build cannot really work.
  --
  -- I.e. we don't want trigger unknown field warning
  cppOptions <- monoidalFieldAla "cpp-options" (alaList' NoCommaFSep Token') L.cppOptions
  asmOptions <-
    monoidalFieldAla "asm-options" (alaList' NoCommaFSep Token') L.asmOptions
      ^^^ availableSince CabalSpecV3_0 []
  cmmOptions <-
    monoidalFieldAla "cmm-options" (alaList' NoCommaFSep Token') L.cmmOptions
      ^^^ availableSince CabalSpecV3_0 []
  ccOptions <- monoidalFieldAla "cc-options" (alaList' NoCommaFSep Token') L.ccOptions
  cxxOptions <-
    monoidalFieldAla "cxx-options" (alaList' NoCommaFSep Token') L.cxxOptions
      ^^^ availableSince CabalSpecV2_2 []
  jsppOptions <-
    monoidalFieldAla "jspp-options" (alaList' NoCommaFSep Token') L.jsppOptions
      ^^^ availableSince CabalSpecV3_16 []
  ldOptions <- monoidalFieldAla "ld-options" (alaList' NoCommaFSep Token') L.ldOptions
  hsc2hsOptions <-
    monoidalFieldAla "hsc2hs-options" (alaList' NoCommaFSep Token') L.hsc2hsOptions
      ^^^ availableSince CabalSpecV3_6 []
  pkgconfigDepends <- monoidalFieldAla "pkgconfig-depends" (alaList CommaFSep) L.pkgconfigDepends
  frameworks <- monoidalFieldAla "frameworks" (alaList' FSep RelativePathNT) L.frameworks
  extraFrameworkDirs <- monoidalFieldAla "extra-framework-dirs" (alaList' FSep SymbolicPathNT) L.extraFrameworkDirs
  asmSources <-
    monoidalFieldAla "asm-sources" (alaList' VCat SymbolicPathNT) L.asmSources
      ^^^ availableSince CabalSpecV3_0 []
  cmmSources <-
    monoidalFieldAla "cmm-sources" (alaList' VCat SymbolicPathNT) L.cmmSources
      ^^^ availableSince CabalSpecV3_0 []
  cSources <- monoidalFieldAla "c-sources" (alaList' VCat SymbolicPathNT) L.cSources
  cxxSources <-
    monoidalFieldAla "cxx-sources" (alaList' VCat SymbolicPathNT) L.cxxSources
      ^^^ availableSince CabalSpecV2_2 []
  jsSources <- monoidalFieldAla "js-sources" (alaList' VCat SymbolicPathNT) L.jsSources
  hsSourceDirs <- hsSourceDirsGrammar
  otherModules <- monoidalFieldAla "other-modules" formatOtherModules L.otherModules
  virtualModules <-
    monoidalFieldAla "virtual-modules" (alaList' VCat MQuoted) L.virtualModules
      ^^^ availableSince CabalSpecV2_2 []
  autogenModules <-
    monoidalFieldAla "autogen-modules" (alaList' VCat MQuoted) L.autogenModules
      ^^^ availableSince CabalSpecV2_0 []
  defaultLanguage <-
    optionalFieldAla "default-language" MQuoted L.defaultLanguage
      ^^^ availableSince CabalSpecV1_10 Nothing
  otherLanguages <-
    monoidalFieldAla "other-languages" (alaList' FSep MQuoted) L.otherLanguages
      ^^^ availableSince CabalSpecV1_10 []
  defaultExtensions <-
    monoidalFieldAla "default-extensions" (alaList' FSep MQuoted) L.defaultExtensions
      ^^^ availableSince CabalSpecV1_10 []
  otherExtensions <-
    monoidalFieldAla "other-extensions" formatOtherExtensions L.otherExtensions
      ^^^ availableSinceWarn CabalSpecV1_10
  oldExtensions <-
    monoidalFieldAla "extensions" (alaList' FSep MQuoted) L.oldExtensions
      ^^^ deprecatedSince
        CabalSpecV1_12
        "Please use 'default-extensions' or 'other-extensions' fields."
      ^^^ removedIn
        CabalSpecV3_0
        "Please use 'default-extensions' or 'other-extensions' fields."
  extraLibs <- monoidalFieldAla "extra-libraries" (alaList' VCat Token) L.extraLibs
  extraLibsStatic <-
    monoidalFieldAla "extra-libraries-static" (alaList' VCat Token) L.extraLibsStatic
      ^^^ availableSince CabalSpecV3_8 []
  extraGHCiLibs <- monoidalFieldAla "extra-ghci-libraries" (alaList' VCat Token) L.extraGHCiLibs
  extraBundledLibs <- monoidalFieldAla "extra-bundled-libraries" (alaList' VCat Token) L.extraBundledLibs
  extraLibFlavours <- monoidalFieldAla "extra-library-flavours" (alaList' VCat Token) L.extraLibFlavours
  extraDynLibFlavours <-
    monoidalFieldAla "extra-dynamic-library-flavours" (alaList' VCat Token) L.extraDynLibFlavours
      ^^^ availableSince CabalSpecV3_0 []
  extraLibDirs <- monoidalFieldAla "extra-lib-dirs" (alaList' FSep SymbolicPathNT) L.extraLibDirs
  extraLibDirsStatic <-
    monoidalFieldAla "extra-lib-dirs-static" (alaList' FSep SymbolicPathNT) L.extraLibDirsStatic
      ^^^ availableSince CabalSpecV3_8 []
  includeDirs <- monoidalFieldAla "include-dirs" (alaList' FSep SymbolicPathNT) L.includeDirs
  includes <- monoidalFieldAla "includes" (alaList' FSep SymbolicPathNT) L.includes
  autogenIncludes <-
    monoidalFieldAla "autogen-includes" (alaList' FSep RelativePathNT) L.autogenIncludes
      ^^^ availableSince CabalSpecV3_0 []
  installIncludes <- monoidalFieldAla "install-includes" (alaList' FSep RelativePathNT) L.installIncludes
  options <- optionsFieldGrammar
  profOptions <- profOptionsFieldGrammar
  sharedOptions <- sharedOptionsFieldGrammar
  profSharedOptions <- profSharedOptionsFieldGrammar
  let staticOptions = mempty
  customFieldsBI <- prefixedFields "x-" L.customFieldsBI
  targetBuildDepends <- monoidalFieldAla "build-depends" formatDependencyList L.targetBuildDepends
  mixins <-
    monoidalFieldAla "mixins" formatMixinList L.mixins
      ^^^ availableSince CabalSpecV2_0 []
  pure BuildInfo{..}
#endif
{-# SPECIALIZE buildInfoFieldGrammar :: ParsecFieldGrammar' BuildInfo #-}
{-# SPECIALIZE buildInfoFieldGrammar :: PrettyFieldGrammar' BuildInfo #-}

hsSourceDirsGrammar
  :: ( FieldGrammar c g
     , c (List FSep (SymbolicPathNT Pkg (Dir Source)) (SymbolicPath Pkg (Dir Source)))
     )
  => g BuildInfo [SymbolicPath Pkg (Dir Source)]
hsSourceDirsGrammar =
  (++)
    <$> monoidalFieldAla "hs-source-dirs" formatHsSourceDirs L.hsSourceDirs
    <*> monoidalFieldAla "hs-source-dir" (alaList' FSep SymbolicPathNT) wrongLens
      --- https://github.com/haskell/cabal/commit/49e3cdae3bdf21b017ccd42e66670ca402e22b44
      ^^^ deprecatedSince CabalSpecV1_2 "Please use 'hs-source-dirs'"
      ^^^ removedIn CabalSpecV3_0 "Please use 'hs-source-dirs' field."
  where
    -- TODO: make pretty printer aware of CabalSpecVersion
    wrongLens :: Functor f => LensLike' f BuildInfo [SymbolicPath Pkg (Dir Source)]
    wrongLens f bi = (\fps -> set L.hsSourceDirs fps bi) <$> f []

optionsFieldGrammar
  :: (FieldGrammar c g, c (List NoCommaFSep Token' Text))
  => g BuildInfo (PerCompilerFlavor [Text])
optionsFieldGrammar =
  PerCompilerFlavor
    <$> monoidalFieldAla "ghc-options" (alaList' NoCommaFSep Token') (extract GHC)
    <*> monoidalFieldAla "ghcjs-options" (alaList' NoCommaFSep Token') (extract GHCJS)
    -- NOTE: Hugs, NHC and JHC are not supported anymore, but these
    -- fields are kept around so that we can still parse legacy .cabal
    -- files that have them.
    <* knownField "jhc-options"
    <* knownField "hugs-options"
    <* knownField "nhc98-options"
    -- NOTE: These are used by MicroCabal for MicroHs
    <* knownField "mhs-options"
  where
    extract :: CompilerFlavor -> ALens' BuildInfo [Text]
    extract flavor = L.options . lookupLens flavor

profOptionsFieldGrammar
  :: (FieldGrammar c g, c (List NoCommaFSep Token' Text))
  => g BuildInfo (PerCompilerFlavor [Text])
profOptionsFieldGrammar =
  PerCompilerFlavor
    <$> monoidalFieldAla "ghc-prof-options" (alaList' NoCommaFSep Token') (extract GHC)
    <*> monoidalFieldAla "ghcjs-prof-options" (alaList' NoCommaFSep Token') (extract GHCJS)
  where
    extract :: CompilerFlavor -> ALens' BuildInfo [Text]
    extract flavor = L.profOptions . lookupLens flavor

sharedOptionsFieldGrammar
  :: (FieldGrammar c g, c (List NoCommaFSep Token' Text))
  => g BuildInfo (PerCompilerFlavor [Text])
sharedOptionsFieldGrammar =
  PerCompilerFlavor
    <$> monoidalFieldAla "ghc-shared-options" (alaList' NoCommaFSep Token') (extract GHC)
    <*> monoidalFieldAla "ghcjs-shared-options" (alaList' NoCommaFSep Token') (extract GHCJS)
  where
    extract :: CompilerFlavor -> ALens' BuildInfo [Text]
    extract flavor = L.sharedOptions . lookupLens flavor

profSharedOptionsFieldGrammar
  :: (FieldGrammar c g, c (List NoCommaFSep Token' Text))
  => g BuildInfo (PerCompilerFlavor [Text])
profSharedOptionsFieldGrammar =
  PerCompilerFlavor
    <$> monoidalFieldAla "ghc-prof-shared-options" (alaList' NoCommaFSep Token') (extract GHC)
      ^^^ availableSince CabalSpecV3_14 []
    <*> monoidalFieldAla "ghcjs-prof-shared-options" (alaList' NoCommaFSep Token') (extract GHCJS)
      ^^^ availableSince CabalSpecV3_14 []
  where
    extract :: CompilerFlavor -> ALens' BuildInfo [Text]
    extract flavor = L.profSharedOptions . lookupLens flavor

lookupLens :: (Functor f, Monoid v) => CompilerFlavor -> LensLike' f (PerCompilerFlavor v) v
lookupLens k f p@(PerCompilerFlavor ghc ghcjs)
  | k == GHC = (`PerCompilerFlavor` ghcjs) <$> f ghc
  | k == GHCJS = (ghc `PerCompilerFlavor`) <$> f ghcjs
  | otherwise = p <$ f mempty

-------------------------------------------------------------------------------
-- Flag
-------------------------------------------------------------------------------

flagFieldGrammar
  :: FieldGrammar c g
  => FlagName
  -> g PackageFlag PackageFlag
#ifdef __MHS__
-- MicroHs does not implement ApplicativeDo, so the do-block below is
-- spelled out with explicit Applicative combinators. Semantics are
-- identical to the ApplicativeDo desugaring GHC performs.
flagFieldGrammar flagName =
  MkPackageFlag flagName
    <$> (T.unpack <$> freeTextFieldDef "description" L.flagDescription)
    <*> booleanFieldDef "default" L.flagDefault True
    <*> booleanFieldDef "manual" L.flagManual False
#else
flagFieldGrammar flagName = do
  flagDescription <- T.unpack <$> freeTextFieldDef "description" L.flagDescription
  flagDefault <- booleanFieldDef "default" L.flagDefault True
  flagManual <- booleanFieldDef "manual" L.flagManual False
  pure MkPackageFlag{..}
#endif
{-# SPECIALIZE flagFieldGrammar :: FlagName -> ParsecFieldGrammar' PackageFlag #-}
{-# SPECIALIZE flagFieldGrammar :: FlagName -> PrettyFieldGrammar' PackageFlag #-}

-------------------------------------------------------------------------------
-- SourceRepo
-------------------------------------------------------------------------------

sourceRepoFieldGrammar
  :: (FieldGrammar c g, c (Identity RepoType))
  => RepoKind
  -> g SourceRepo SourceRepo
#ifdef __MHS__
-- MicroHs does not implement ApplicativeDo, so the do-block below is
-- spelled out with explicit Applicative combinators. Semantics are
-- identical to the ApplicativeDo desugaring GHC performs.
sourceRepoFieldGrammar repoKind =
  SourceRepo repoKind
    <$> optionalField "type" L.repoType
    <*> freeTextField "location" L.repoLocation
    <*> optionalFieldAla "module" Token L.repoModule
    <*> optionalFieldAla "branch" Token L.repoBranch
    <*> optionalFieldAla "tag" Token L.repoTag
    <*> optionalFieldAla "subdir" FilePathNT L.repoSubdir
#else
sourceRepoFieldGrammar repoKind = do
  repoType <- optionalField "type" L.repoType
  repoLocation <- freeTextField "location" L.repoLocation
  repoModule <- optionalFieldAla "module" Token L.repoModule
  repoBranch <- optionalFieldAla "branch" Token L.repoBranch
  repoTag <- optionalFieldAla "tag" Token L.repoTag
  repoSubdir <- optionalFieldAla "subdir" FilePathNT L.repoSubdir
  pure SourceRepo{..}
#endif
{-# SPECIALIZE sourceRepoFieldGrammar :: RepoKind -> ParsecFieldGrammar' SourceRepo #-}
{-# SPECIALIZE sourceRepoFieldGrammar :: RepoKind -> PrettyFieldGrammar' SourceRepo #-}

-------------------------------------------------------------------------------
-- SetupBuildInfo
-------------------------------------------------------------------------------

setupBInfoFieldGrammar
  :: (FieldGrammar c g, c (List CommaVCat (Identity Dependency) Dependency))
  => Bool
  -> g SetupBuildInfo SetupBuildInfo
setupBInfoFieldGrammar def =
  flip SetupBuildInfo def
    <$> monoidalFieldAla "setup-depends" (alaList CommaVCat) L.setupDepends
{-# SPECIALIZE setupBInfoFieldGrammar :: Bool -> ParsecFieldGrammar' SetupBuildInfo #-}
{-# SPECIALIZE setupBInfoFieldGrammar :: Bool -> PrettyFieldGrammar' SetupBuildInfo #-}

-------------------------------------------------------------------------------
-- Define how field values should be formatted for 'pretty'.
-------------------------------------------------------------------------------

formatDependencyList :: [Dependency] -> List CommaVCat (Identity Dependency) Dependency
formatDependencyList = alaList CommaVCat

formatMixinList :: [Mixin] -> List CommaVCat (Identity Mixin) Mixin
formatMixinList = alaList CommaVCat

formatExtraSourceFiles :: [RelativePath Pkg File] -> List VCat (RelativePathNT Pkg File) (RelativePath Pkg File)
formatExtraSourceFiles = alaList' VCat RelativePathNT

formatExposedModules :: [ModuleName] -> List VCat (MQuoted ModuleName) ModuleName
formatExposedModules = alaList' VCat MQuoted

formatHsSourceDirs :: [SymbolicPath Pkg (Dir Source)] -> List FSep (SymbolicPathNT Pkg (Dir Source)) (SymbolicPath Pkg (Dir Source))
formatHsSourceDirs = alaList' FSep SymbolicPathNT

formatOtherExtensions :: [Extension] -> List FSep (MQuoted Extension) Extension
formatOtherExtensions = alaList' FSep MQuoted

formatOtherModules :: [ModuleName] -> List VCat (MQuoted ModuleName) ModuleName
formatOtherModules = alaList' VCat MQuoted

-------------------------------------------------------------------------------
-- newtypes
-------------------------------------------------------------------------------

-- | Newtype for data directory (absolute or relative).
--
-- Accepts empty file path, but issues a warning;
-- there are simply too many (~1200) package definition files
--
-- @
-- data-dir: ""
-- @
--
-- across Hackage to outrule them completely.
-- I suspect some of them are generated (e.g. formatted) by machine.
newtype CompatDataDir = CompatDataDir {getCompatDataDir :: SymbolicPath Pkg (Dir DataDir)}

instance Parsec CompatDataDir where
  parsec = do
    token <- parsecToken
    when (null token) $
      parsecWarning PWTEmptyFilePath "empty FilePath"
    return (CompatDataDir $ makeSymbolicPath token)

instance Pretty CompatDataDir where
  pretty = showToken . getSymbolicPath . getCompatDataDir

newtype CompatLicenseFile = CompatLicenseFile {getCompatLicenseFile :: [RelativePath Pkg File]}

-- TODO
instance Parsec CompatLicenseFile where
  parsec = emptyToken <|> CompatLicenseFile . (coerce :: List FSep (Identity a) a -> [a]) <$> parsec
    where
      emptyToken = P.try $ do
        token <- parsecToken
        if null token
          then return (CompatLicenseFile [])
          else P.unexpected "non-empty-token"

instance Pretty CompatLicenseFile where
  pretty = pretty . (coerce :: [a] -> List FSep (Identity a) a) . getCompatLicenseFile

-------------------------------------------------------------------------------
-- vim syntax definitions
-------------------------------------------------------------------------------

-- | '_syntaxFieldNames' and '_syntaxExtensions'
-- are for generating VIM syntax file definitions.
_syntaxFieldNames :: IO ()
_syntaxFieldNames =
  sequence_
    [ BS8.putStrLn $ " \\ " <> n
    | n <-
        ordNub $
          sort $
            mconcat
              [ fieldGrammarKnownFieldList packageDescriptionFieldGrammar
              , fieldGrammarKnownFieldList $ libraryFieldGrammar LMainLibName
              , fieldGrammarKnownFieldList $ executableFieldGrammar "exe"
              , fieldGrammarKnownFieldList $ foreignLibFieldGrammar "flib"
              , fieldGrammarKnownFieldList testSuiteFieldGrammar
              , fieldGrammarKnownFieldList benchmarkFieldGrammar
              , fieldGrammarKnownFieldList $ flagFieldGrammar (error "flagname")
              , fieldGrammarKnownFieldList $ sourceRepoFieldGrammar (error "repokind")
              , fieldGrammarKnownFieldList $ setupBInfoFieldGrammar True
              ]
    ]

_syntaxExtensions :: IO ()
_syntaxExtensions =
  sequence_
    [ putStrLn $ "  \\ " <> e
    | e <-
        ["Safe", "Trustworthy", "Unsafe"]
          ++ es
          ++ map ("No" ++) es
    ]
  where
    es =
      ordNub $
        sort
          [ prettyShow e
          | e <- [minBound .. maxBound]
          , e `notElem` [Safe, Unsafe, Trustworthy]
          ]
