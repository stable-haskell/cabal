{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.BuildInfo
  ( BuildInfo (..)
  , emptyBuildInfo
  , allLanguages
  , allExtensions
  , usedExtensions
  , usesTemplateHaskellOrQQ
  , hcOptions
  , hcProfOptions
  , hcSharedOptions
  , hcProfSharedOptions
  , hcStaticOptions
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Types.Dependency
import Distribution.Types.ExeDependency
import Distribution.Types.ExtraSource
import Distribution.Types.LegacyExeDependency
import Distribution.Types.Mixin
import Distribution.Types.PkgconfigDependency
import Distribution.Utils.Path

import Distribution.Compiler
import Distribution.ModuleName
import Language.Haskell.Extension

-- Consider refactoring into executable and library versions.
data BuildInfo = BuildInfo
  { buildable :: Bool
  -- ^ component is buildable here
  , buildTools :: [LegacyExeDependency]
  -- ^ Tools needed to build this bit.
  --
  -- This is a legacy field that 'buildToolDepends' largely supersedes.
  --
  -- Unless use are very sure what you are doing, use the functions in
  -- "Distribution.Simple.BuildToolDepends" rather than accessing this
  -- field directly.
  , buildToolDepends :: [ExeDependency]
  -- ^ Haskell tools needed to build this bit
  --
  -- This field is better than 'buildTools' because it allows one to
  -- precisely specify an executable in a package.
  --
  -- Unless use are very sure what you are doing, use the functions in
  -- "Distribution.Simple.BuildToolDepends" rather than accessing this
  -- field directly.
  , cppOptions :: [String]
  -- ^ options for pre-processing Haskell code
  , asmOptions :: [String]
  -- ^ options for assembler
  , cmmOptions :: [String]
  -- ^ options for C-- compiler
  , ccOptions :: [String]
  -- ^ options for C compiler
  , cxxOptions :: [String]
  -- ^ options for C++ compiler
  , ldOptions :: [String]
  -- ^ options for linker
  , hsc2hsOptions :: [String]
  -- ^ options for hsc2hs
  , pkgconfigDepends :: [PkgconfigDependency]
  -- ^ pkg-config packages that are used
  , frameworks :: [RelativePath Framework File]
  -- ^ support frameworks for Mac OS X
  , extraFrameworkDirs :: [SymbolicPath Pkg (Dir Framework)]
  -- ^ extra locations to find frameworks
  , asmSources :: [ExtraSource Pkg]
  -- ^ Assembly source files
  , cmmSources :: [ExtraSource Pkg]
  -- ^ C-- source files
  , autogenCmmSources :: [ExtraSource Build]
  -- ^ C-- generated source files
  , cSources :: [ExtraSource Pkg]
  -- ^ C source files
  , cxxSources :: [ExtraSource Pkg]
  -- ^ C++ source files
  , jsSources :: [ExtraSource Pkg]
  -- ^ JavaScript source file
  , hsSourceDirs :: [SymbolicPath Pkg (Dir Source)]
  -- ^ where to look for the Haskell module hierarchy
  , -- NB: these are symbolic paths are not relative paths,
    -- because autogenerated modules might end up in an absolute path
    otherModules :: [ModuleName]
  -- ^ non-exposed or non-main modules
  , virtualModules :: [ModuleName]
  -- ^ exposed modules that do not have a source file (e.g. @GHC.Prim@ from @ghc-prim@ package)
  , autogenModules :: [ModuleName]
  -- ^ not present on sdist, Paths_* or user-generated with a custom Setup.hs
  , defaultLanguage :: Maybe Language
  -- ^ language used when not explicitly specified
  , otherLanguages :: [Language]
  -- ^ other languages used within the package
  , defaultExtensions :: [Extension]
  -- ^ language extensions used by all modules
  , otherExtensions :: [Extension]
  -- ^ other language extensions used within the package
  , oldExtensions :: [Extension]
  -- ^ the old extensions field, treated same as 'defaultExtensions'
  , extraLibs :: [String]
  -- ^ what libraries to link with when compiling a program that uses your package
  , extraLibsStatic :: [String]
  -- ^ what libraries to link with when compiling a program fully statically that uses your package
  , extraGHCiLibs :: [String]
  -- ^ if present, overrides extraLibs when package is loaded with GHCi.
  , extraBundledLibs :: [String]
  -- ^ if present, adds libs to hs-libraries, which become part of the package.
  --   Example 1: the Cffi library shipping with the rts, alongside the HSrts-1.0.a,.o,...
  --   Example 2: a library that is being built by a foreign tool (e.g. rust)
  --              and copied and registered together with this library.  The
  --              logic on how this library is built will have to be encoded in a
  --              custom Setup for now.  Otherwise cabal would need to learn how to
  --              call arbitrary library builders.
  , extraLibFlavours :: [String]
  -- ^ Hidden Flag.  This set of strings, will be appended to all libraries when
  --   copying. E.g. [libHS<name>_<flavour> | flavour <- extraLibFlavours]. This
  --   should only be needed in very specific cases, e.g. the `rts` package, where
  --   there are multiple copies of slightly differently built libs.
  , extraDynLibFlavours :: [String]
  -- ^ Hidden Flag. This set of strings will be appended to all /dynamic/
  --   libraries when copying. This is particularly useful with the `rts` package,
  --   where we want different dynamic flavours of the RTS library to be installed.
  , extraLibDirs :: [SymbolicPath Pkg (Dir Lib)]
  , extraLibDirsStatic :: [SymbolicPath Pkg (Dir Lib)]
  , includeDirs :: [SymbolicPath Pkg (Dir Include)]
  -- ^ directories to find .h files
  , includes :: [SymbolicPath Include File]
  -- ^ The .h files to be found in includeDirs
  , autogenIncludes :: [RelativePath Include File]
  -- ^ The .h files to be generated (e.g. by @autoconf@)
  , installIncludes :: [RelativePath Include File]
  -- ^ .h files to install with the package
  , options :: PerCompilerFlavor [String]
  , profOptions :: PerCompilerFlavor [String]
  , sharedOptions :: PerCompilerFlavor [String]
  , profSharedOptions :: PerCompilerFlavor [String]
  , staticOptions :: PerCompilerFlavor [String]
  , customFieldsBI :: [(String, String)]
  -- ^ Custom fields starting
  --  with x-, stored in a
  --  simple assoc-list.
  , targetBuildDepends :: [Dependency]
  -- ^ Dependencies specific to a library or executable target
  , mixins :: [Mixin]
  }
  deriving (Generic, Show, Read, Eq, Ord, Data)

instance Binary BuildInfo
instance Structured BuildInfo
instance NFData BuildInfo where rnf = genericRnf

instance Monoid BuildInfo where
  mempty =
    BuildInfo
      { buildable = True
      , buildTools = []
      , buildToolDepends = []
      , cppOptions = []
      , asmOptions = []
      , cmmOptions = []
      , ccOptions = []
      , cxxOptions = []
      , ldOptions = []
      , hsc2hsOptions = []
      , pkgconfigDepends = []
      , frameworks = []
      , extraFrameworkDirs = []
      , asmSources = []
      , cmmSources = []
      , autogenCmmSources = []
      , cSources = []
      , cxxSources = []
      , jsSources = []
      , hsSourceDirs = []
      , otherModules = []
      , virtualModules = []
      , autogenModules = []
      , defaultLanguage = Nothing
      , otherLanguages = []
      , defaultExtensions = []
      , otherExtensions = []
      , oldExtensions = []
      , extraLibs = []
      , extraLibsStatic = []
      , extraGHCiLibs = []
      , extraBundledLibs = []
      , extraLibFlavours = []
      , extraDynLibFlavours = []
      , extraLibDirs = []
      , extraLibDirsStatic = []
      , includeDirs = []
      , includes = []
      , autogenIncludes = []
      , installIncludes = []
      , options = mempty
      , profOptions = mempty
      , sharedOptions = mempty
      , profSharedOptions = mempty
      , staticOptions = mempty
      , customFieldsBI = []
      , targetBuildDepends = []
      , mixins = []
      }
  mappend = (<>)

instance Semigroup BuildInfo where
  a <> b =
    BuildInfo
      { buildable = buildable a && buildable b
      , buildTools = combine buildTools
      , buildToolDepends = combine buildToolDepends
      , cppOptions = combine cppOptions
      , asmOptions = combine asmOptions
      , cmmOptions = combine cmmOptions
      , ccOptions = combine ccOptions
      , cxxOptions = combine cxxOptions
      , ldOptions = combine ldOptions
      , hsc2hsOptions = combine hsc2hsOptions
      , pkgconfigDepends = combine pkgconfigDepends
      , frameworks = combineNub frameworks
      , extraFrameworkDirs = combineNub extraFrameworkDirs
      , asmSources = combineNub asmSources
      , cmmSources = combineNub cmmSources
      , autogenCmmSources = combineNub autogenCmmSources
      , cSources = combineNub cSources
      , cxxSources = combineNub cxxSources
      , jsSources = combineNub jsSources
      , hsSourceDirs = combineNub hsSourceDirs
      , otherModules = combineNub otherModules
      , virtualModules = combineNub virtualModules
      , autogenModules = combineNub autogenModules
      , defaultLanguage = combineMby defaultLanguage
      , otherLanguages = combineNub otherLanguages
      , defaultExtensions = combineNub defaultExtensions
      , otherExtensions = combineNub otherExtensions
      , oldExtensions = combineNub oldExtensions
      , extraLibs = combine extraLibs
      , extraLibsStatic = combine extraLibsStatic
      , extraGHCiLibs = combine extraGHCiLibs
      , extraBundledLibs = combine extraBundledLibs
      , extraLibFlavours = combine extraLibFlavours
      , extraDynLibFlavours = combine extraDynLibFlavours
      , extraLibDirs = combineNub extraLibDirs
      , extraLibDirsStatic = combineNub extraLibDirsStatic
      , includeDirs = combineNub includeDirs
      , includes = combineNub includes
      , autogenIncludes = combineNub autogenIncludes
      , installIncludes = combineNub installIncludes
      , options = combine options
      , profOptions = combine profOptions
      , sharedOptions = combine sharedOptions
      , profSharedOptions = combine profSharedOptions
      , staticOptions = combine staticOptions
      , customFieldsBI = combine customFieldsBI
      , targetBuildDepends = combineNub targetBuildDepends
      , mixins = combine mixins
      }
    where
      combine field = field a `mappend` field b
      combineNub field = nub (combine field)
      combineMby field = field b `mplus` field a

emptyBuildInfo :: BuildInfo
emptyBuildInfo = mempty

-- | The 'Language's used by this component
allLanguages :: BuildInfo -> [Language]
allLanguages bi =
  maybeToList (defaultLanguage bi)
    ++ otherLanguages bi

-- | The 'Extension's that are used somewhere by this component
allExtensions :: BuildInfo -> [Extension]
allExtensions bi =
  usedExtensions bi
    ++ otherExtensions bi

-- | The 'Extensions' that are used by all modules in this component
usedExtensions :: BuildInfo -> [Extension]
usedExtensions bi =
  oldExtensions bi
    ++ defaultExtensions bi

-- | Whether any modules in this component use Template Haskell or
-- Quasi Quotes
usesTemplateHaskellOrQQ :: BuildInfo -> Bool
usesTemplateHaskellOrQQ bi = any p (allExtensions bi)
  where
    p ex =
      ex
        `elem` [EnableExtension TemplateHaskell, EnableExtension QuasiQuotes]

-- | Select options for a particular Haskell compiler.
hcOptions :: CompilerFlavor -> BuildInfo -> [String]
hcOptions = lookupHcOptions options

hcProfOptions :: CompilerFlavor -> BuildInfo -> [String]
hcProfOptions = lookupHcOptions profOptions

hcSharedOptions :: CompilerFlavor -> BuildInfo -> [String]
hcSharedOptions = lookupHcOptions sharedOptions

hcProfSharedOptions :: CompilerFlavor -> BuildInfo -> [String]
hcProfSharedOptions = lookupHcOptions profSharedOptions

hcStaticOptions :: CompilerFlavor -> BuildInfo -> [String]
hcStaticOptions = lookupHcOptions staticOptions

lookupHcOptions
  :: (BuildInfo -> PerCompilerFlavor [String])
  -> CompilerFlavor
  -> BuildInfo
  -> [String]
lookupHcOptions f hc bi = case f bi of
  PerCompilerFlavor ghc ghcjs
    | hc == GHC -> ghc
    | hc == GHCJS -> ghcjs
    | otherwise -> mempty
