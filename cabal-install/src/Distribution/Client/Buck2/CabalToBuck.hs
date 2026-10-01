-- | Turns one local package's already-resolved 'PackageDescription' (flags
-- and conditionals already flattened by the solver, so gated
-- @cxx-sources@\/@ghc-options@\/etc. from an @if flag(...)@ stanza show up
-- here exactly as they should for the resolved build) into the buck2 rule
-- calls for its @BUCK.cabal.bzl@ file: 'haskell_library' \/ 'haskell_binary'
-- \/ 'haskell_test' for each buildable component, plus a 'cxx_library' for
-- any component with @cxx-sources@\/@c-sources@ and an
-- @external_pkgconfig_library@ for each distinct @pkgconfig-depends@.
module Distribution.Client.Buck2.CabalToBuck
  ( LocalPackageIndex
  , PackageTargets (..)
  , generatePackageTargets
  , libTargetName
  , AutogenExport (..)
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((<.>), (</>), joinPath, normalise, splitDirectories, takeDirectory, takeExtension)
import Data.List (stripPrefix)

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Distribution.Compat.NonEmptySet as NES
import Distribution.Compiler (CompilerFlavor (GHC))
import qualified Distribution.ModuleName as ModuleName
import Distribution.Package (packageName, packageVersion)
import Distribution.PackageDescription
  ( Benchmark (benchmarkInterface, benchmarkName)
  , BenchmarkInterface (..)
  , BuildInfo
  , Executable (exeName, modulePath)
  , Library (exposedModules, libBuildInfo, libName)
  , LibraryName (..)
  , PackageDescription
  , TestSuite (testInterface, testName)
  , TestSuiteInterface (..)
  , autogenModules
  , buildToolDepends
  , cppOptions
  , ccOptions
  , cxxOptions
  , asmSources
  , cmmSources
  , cxxSources
  , cSources
  , usedExtensions
  , defaultLanguage
  , extraLibs
  , ldOptions
  , hcOptions
  , buildType
  , hsSourceDirs
  , lookupComponent
  , updatePackageDescription
  , includeDirs
  , otherModules
  , pkgBuildableComponents
  , pkgconfigDepends
  , targetBuildDepends
  )
import Distribution.Types.Component (Component (..), componentBuildInfo, componentName)
import Distribution.Backpack (OpenModule (..), OpenUnitId (..))
import Distribution.Types.ComponentLocalBuildInfo (ComponentLocalBuildInfo (componentPackageDeps, componentUnitId), maybeComponentExposedModules)
import Distribution.Types.ExposedModule (ExposedModule (..))
import Distribution.Types.MungedPackageId (MungedPackageId (mungedName))
import Distribution.Types.MungedPackageName (MungedPackageName (..))
import Distribution.Types.UnitId (unDefUnitId)
import Distribution.Types.ComponentName (ComponentName (..))
import Distribution.Types.Dependency (depLibraries, depPkgName)
import Distribution.Types.ExeDependency (ExeDependency (..))
import Distribution.Types.LocalBuildInfo (LocalBuildInfo, componentNameCLBIs, installedPkgs, localPkgDescr, withPrograms)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.InstalledPackageInfo (InstalledPackageInfo (sourceLibName))
import Distribution.Simple.Program (ghcProgram, lookupProgram, programOverrideArgs)
import Distribution.Types.PackageName (PackageName, unPackageName)
import Distribution.Types.PkgconfigDependency (PkgconfigDependency (..))
import Distribution.Types.PkgconfigName (unPkgconfigName)
import Distribution.Types.UnqualComponentName (unUnqualComponentName)
import Distribution.Utils.Path (getSymbolicPath, interpretSymbolicPath)
import Distribution.Verbosity (VerbosityFlags (vLevel), VerbosityLevel (Silent), modifyVerbosityFlags)

import Distribution.Simple.Build.Macros (generateCabalMacrosHeader)
import Distribution.Simple.Build.PathsModule (generatePathsModule)
import Distribution.Simple.Build.PackageInfoModule (generatePackageInfoModule)
import Distribution.Simple.BuildPaths (autogenPackageInfoModuleName, autogenPathsModuleName)
import Distribution.Simple.Utils (findHookedPackageDesc, ordNub, warn)
import Distribution.Simple.PackageDescription (readHookedBuildInfo)
import Distribution.Simple.LocalBuildInfo (buildDir, mbWorkDirLBI)
import Distribution.Types.BuildType (BuildType (Configure))
import Distribution.Types.HookedBuildInfo (HookedBuildInfo, emptyHookedBuildInfo)

import Distribution.Client.Buck2.Starlark
import Distribution.Client.Buck2.Variant

-- | Maps every local project package's name to the buck2 cell-relative
-- directory its @BUCK@ file lives in (@.@ for one at the project root), so
-- a dependency on another local package can be turned into a fully
-- qualified target label - plus the set of other packages its main
-- library's @reexported-modules@ re-export from (see 'classifyDeps').
type LocalPackageIndex = Map PackageName (FilePath, [PackageName])

-- | The generated rule calls for one package, plus the @load@ statements
-- they need at the top of the file, plus every generated autogen file
-- (a component's @cabal_macros.h@, or the package's own @Paths_\<pkg\>@
-- module) that needs an @export_file()@ entry in @cabal-buck2\/autogen\/
-- BUCK@ (see 'Distribution.Client.Buck2.Generate') so it can be
-- referenced - by a generated rule's own @cabal_component@ kwarg, or by a
-- hand-written rule elsewhere in the project - as a real, buck2-tracked
-- target instead of an untracked path string. Each entry is
-- @(exportTargetName, pathRelativeToCabalBuck2Autogen)@; 'writeMacrosHeader'
-- and 'writePathsModule' are the only producers.
data PackageTargets = PackageTargets
  { ptLoads :: [(String, [String])]
  , ptCalls :: [Call]
  , ptAutogenExports :: [AutogenExport]
  }

-- | A target of the package's @cabal-buck2/autogen/BUCK@ file: a file
-- (@export_file@, path relative to the autogen directory) or a directory
-- of headers (@filegroup@, entries @(path in the output, path relative
-- to the autogen directory)@ - see Note [Configure build type]).
data AutogenExport
  = AutogenFile String FilePath
  | AutogenDir String [(String, FilePath)]
  deriving (Eq, Ord, Show)

instance Semigroup PackageTargets where
  PackageTargets l1 c1 e1 <> PackageTargets l2 c2 e2 =
    PackageTargets (foldl' addLoad l1 l2) (c1 ++ c2) (ordNub (e1 ++ e2))
    where
      addLoad acc (tgt, names) = case lookup tgt acc of
        Nothing -> acc ++ [(tgt, names)]
        Just _ -> map (\(t, ns) -> if t == tgt then (t, ordNub (ns ++ names)) else (t, ns)) acc

instance Monoid PackageTargets where
  mempty = PackageTargets [] [] []

-- | Generate the buck2 targets for every buildable component of a local
-- package. @pkgDir@ is the package's own directory (where the generated
-- @BUCK.cabal.bzl@ will live) - every emitted path is relative to it.
generatePackageTargets
  :: Verbosity
  -> Variant
  -> LocalPackageIndex
  -> FilePath
  -- ^ @pkgDir@'s own buck2 cell-relative directory (@.@ at the project
  -- root) - used only to locate the generated @cabal_macros.h@ from the
  -- compile action's working directory (the project root), distinct from
  -- @pkgDir@ itself (a real filesystem path, used for everything else).
  -> Map (PackageName, ComponentName) LocalBuildInfo
  -> Set String
  -- ^ Every @build-tool-depends:@ executable name
  -- "Distribution.Client.Buck2.Prebuilt" resolved a real *external*
  -- binary for (and generated an @export_file()@ target for) - see
  -- 'buildToolDependsArg'.
  -> FilePath
  -> PackageDescription
  -> IO PackageTargets
generatePackageTargets verbosity variant localIndex rootRelPkgDir componentLBIs externalBuildTools pkgDir pkgDesc = do
  hbi <- hookedBuildInfo verbosity pkgDesc componentLBIs
  -- Computed up front (silently - see 'skippedLibraries's own haddock),
  -- so every component below - regardless of its own textual position
  -- in the .cabal file relative to the library it depends on - already
  -- knows which of this package's own libraries won't get a rule, and
  -- can skip itself too instead of emitting a dangling dependency edge.
  skippedLibs <- skippedLibraries verbosity variant pkgDesc rootRelPkgDir pkgDir componentLBIs
  targets <-
    mconcat
      <$> traverse
        (generateComponent verbosity variant localIndex rootRelPkgDir componentLBIs externalBuildTools pkgDir pkgDesc hbi skippedLibs)
        (pkgBuildableComponents pkgDesc)
  return targets{ptCalls = dedupPkgconfigCalls (ptCalls targets)}

-- | The @.buildinfo@ that a build-type Configure package's configure
-- script wrote when cabal buck2 configured the package (it runs in the
-- component's build directory); empty for other packages. Cabal applies
-- it to the package description at build time ('updatePackageDescription'),
-- which is what 'generateComponent' does too: extra-libraries,
-- include-dirs, ... come from it (e.g. GHC's rts and ghc-internal).
hookedBuildInfo :: Verbosity -> PackageDescription -> Map (PackageName, ComponentName) LocalBuildInfo -> IO HookedBuildInfo
hookedBuildInfo verbosity pkgDesc componentLBIs
  | buildType pkgDesc /= Configure = return emptyHookedBuildInfo
  | otherwise = case [lbi | ((pn, _), lbi) <- Map.toList componentLBIs, pn == packageName pkgDesc] of
      [] -> return emptyHookedBuildInfo
      lbi : _ -> do
        let mbWorkDir = mbWorkDirLBI lbi
        exists <- doesDirectoryExist (interpretSymbolicPath mbWorkDir (buildDir lbi))
        if not exists
          then return emptyHookedBuildInfo
          else do
            mfile <- findHookedPackageDesc verbosity mbWorkDir (buildDir lbi)
            maybe (return emptyHookedBuildInfo) (readHookedBuildInfo verbosity mbWorkDir) mfile

-- | Which of this package's own libraries 'generateComponent' is going
-- to skip (unresolvable modules - see 'resolveModules'), found out
-- ahead of the real per-component pass so a *different* component that
-- depends on one (e.g. cabal-testsuite's own @test-runtime-deps@
-- executable, which build-depends on cabal-testsuite's own library) can
-- skip itself too, rather than emitting a rule whose @deps@ references a
-- target that was never generated - buck2 fails that outright at
-- analysis time ("Unknown target"), for the *entire* build, the same
-- class of problem 'resolveModules' itself exists to avoid for a single
-- component's own missing source file.
--
-- Runs with 'silent' verbosity deliberately: it duplicates exactly the
-- same resolution 'generateComponent' below will redo for real (and
-- warn about) when it reaches that library component itself - this
-- pass exists only to know the *outcome* early, not to report it twice.
skippedLibraries :: Verbosity -> Variant -> PackageDescription -> FilePath -> FilePath -> Map (PackageName, ComponentName) LocalBuildInfo -> IO (Set LibraryName)
skippedLibraries verbosity variant pkgDesc rootRelPkgDir pkgDir componentLBIs =
  Set.fromList . catMaybes
    <$> traverse checkLib [lib | CLib lib <- pkgBuildableComponents pkgDesc]
  where
    quiet = modifyVerbosityFlags (\vf -> vf{vLevel = Silent}) verbosity
    checkLib lib = do
      let bi = libBuildInfo lib
      msrcs <- resolveModules quiet variant (variantTargetName variant (libTargetName (packageName pkgDesc) (libName lib))) pkgDesc (lbiClbiFor pkgDesc componentLBIs (CLib lib)) rootRelPkgDir pkgDir bi (exposedModules lib ++ otherModules bi)
      return $ if isNothing msrcs then Just (libName lib) else Nothing

-- | Look up a component's real, Cabal-computed 'LocalBuildInfo' (from
-- "Distribution.Client.CmdBuck2") plus its own 'ComponentLocalBuildInfo'
-- within it (via 'componentNameCLBIs') - 'Nothing' if either lookup fails
-- (e.g. a component that isn't part of the elaborated build plan, such as
-- a test-suite when tests aren't enabled), in which case callers skip the
-- component rather than emit a rule built from fabricated data.
lbiClbiFor :: PackageDescription -> Map (PackageName, ComponentName) LocalBuildInfo -> Component -> Maybe (LocalBuildInfo, ComponentLocalBuildInfo)
lbiClbiFor pkgDesc componentLBIs comp = do
  lbi <- Map.lookup (packageName pkgDesc, componentName comp) componentLBIs
  clbi <- listToMaybe (componentNameCLBIs lbi (componentName comp))
  return (lbi, clbi)

-- | Two components of the *same* package sharing a @pkgconfig-depends@
-- each generate their own @external_pkgconfig_library()@ call (from
-- 'cxxLibraryFor', called once per component) - harmless on its own, but
-- both would declare the same target @name@ in the same
-- @generated_targets()@, which buck2 rejects as a duplicate target. Kept
-- as a post-pass here (rather than threading a running set through
-- component generation) so each component's own generation stays
-- self-contained; cross-*package* duplicates - two different local
-- packages needing the same system library - aren't addressed by this,
-- since each package's calls only ever collide with its own.
dedupPkgconfigCalls :: [Call] -> [Call]
dedupPkgconfigCalls = go []
  where
    go _ [] = []
    go seen (c : cs)
      | callFn c == "external_pkgconfig_library"
      , Just (VStr n) <- lookup "name" (callArgs c) =
          if n `elem` seen then go seen cs else c : go (n : seen) cs
      | otherwise = c : go seen cs

generateComponent
  :: Verbosity
  -> Variant
  -> LocalPackageIndex
  -> FilePath
  -> Map (PackageName, ComponentName) LocalBuildInfo
  -> Set String
  -> FilePath
  -> PackageDescription
  -> HookedBuildInfo
  -> Set LibraryName
  -> Component
  -> IO PackageTargets
generateComponent verbosity variant localIndex rootRelPkgDir componentLBIs externalBuildTools pkgDir pkgDesc hbi skippedLibs comp0 = case comp of
  CLib lib -> library (tname (libTargetName (packageName pkgDesc) (libName lib))) lib
  CExe exe -> ifNotOnSkippedLib (componentBuildInfo comp) (tname (unUnqualComponentName (exeName exe))) "executable" $ executable exe
  CTest test -> ifNotOnSkippedLib (componentBuildInfo comp) (tname (unUnqualComponentName (testName test))) "test-suite" $ testSuite test
  CBench bench -> ifNotOnSkippedLib (componentBuildInfo comp) (tname (unUnqualComponentName (benchmarkName bench))) "benchmark" $ benchmark bench
  CFLib _ -> skip "foreign library (not supported yet)"
  where
    skip why = do
      warn verbosity $ "cabal buck2: skipping " ++ why ++ " in package " ++ unPackageName (packageName pkgDesc)
      return mempty

    -- The component as configured, when the plan has a LocalBuildInfo for
    -- it: a build-type Configure package's configure script adds fields
    -- through its .buildinfo (extra-libraries, include-dirs, ...), which
    -- the plan's own description does not have.
    comp = case lbiClbiFor pkgDesc componentLBIs comp0 of
      Just (lbi, _) | Just c <- lookupComponent (updatePackageDescription hbi (localPkgDescr lbi)) (componentName comp0) -> c
      _ -> fromMaybe comp0 (lookupComponent (updatePackageDescription hbi pkgDesc) (componentName comp0))

    -- Target name in this variant (see Note [Variants]).
    tname = variantTargetName variant

    -- `default_target_platform` of every generated rule in a variant.
    platformArg = [("default_target_platform", str plat) | Just plat <- [variantPlatform variant]]

    -- See Note [Project-level ghc-options]
    projectGhcOptions lbi = maybe [] programOverrideArgs (lookupProgram ghcProgram (withPrograms lbi))

    -- `unit_id` and `package_name` of a library: Cabal's unit id, unless
    -- the .cabal file fixes it with `-this-unit-id` in its ghc-options (the
    -- last flag wins for GHC, so it wins here too); that flag is then
    -- removed from compiler_flags. The registered name is Cabal's: the
    -- package name, or `z-<pkg>-z-<lib>` for a sub-library.
    unitIdArgs lib clbi =
      let (flags, explicit) = splitThisUnitId (hcOptions GHC (libBuildInfo lib))
          uid = fromMaybe (prettyShow (componentUnitId clbi)) explicit
          pn = unPackageName (packageName pkgDesc)
          regName = case libName lib of
            LMainLibName -> pn
            LSubLibName n -> "z-" ++ pn ++ "-z-" ++ unUnqualComponentName n
          -- `package-name`/`lib-name` of the registration, which is how
          -- GHC resolves a unit id of the form `pkg:lib` (GHC's rts ways).
          sublibArgs = case libName lib of
            LMainLibName -> []
            LSubLibName n -> [("cabal_package", str pn), ("lib_name", str (unUnqualComponentName n))]
       in ([("unit_id", str uid), ("package_name", str regName), ("version", str (prettyShow (packageVersion pkgDesc)))] ++ sublibArgs, flags)

    -- A component that build-depends on one of *this same package's*
    -- own libraries, when that library was itself skipped (see
    -- 'skippedLibraries'), can't be built either - it would emit a rule
    -- whose own @deps@ references a target that was never generated,
    -- which buck2 rejects outright ("Unknown target") at analysis time
    -- for the whole build, not just a warning. cabal-testsuite's own
    -- @test-runtime-deps@ executable (build-depends on cabal-testsuite's
    -- own library) is exactly this case.
    ifNotOnSkippedLib bi targetName kind act =
      case [ln | d <- targetBuildDepends bi, depPkgName d == packageName pkgDesc, ln <- NES.toList (depLibraries d), ln `Set.member` skippedLibs] of
        (ln : _) ->
          skip
            ( kind
                ++ " "
                ++ targetName
                ++ " (depends on "
                ++ libTargetName (packageName pkgDesc) ln
                ++ ", itself skipped)"
            )
        [] -> act

    library targetName lib
      | noModules && null (compiledSources (libBuildInfo lib)) = nonHaskellLibrary targetName lib
      | otherwise = case lbiClbiFor pkgDesc componentLBIs comp of
      Nothing -> skip ("library " ++ targetName ++ " (no LocalBuildInfo found for it in the elaborated build plan)")
      Just (lbi, clbi) -> do
        let bi = libBuildInfo lib
        msrcs <- resolveModules verbosity variant targetName pkgDesc (Just (lbi, clbi)) rootRelPkgDir pkgDir bi (exposedModules lib ++ otherModules bi)
        case msrcs of
          Nothing -> skip ("library " ++ targetName ++ " (couldn't resolve all its modules)")
          Just (srcs, srcAutogenExports) -> do
            -- See Note [C and Cmm sources]
            let ghcCompilesC = null (pkgconfigDepends bi)
                ghcSrcs = sourcesWithOptions (map (normalise . getSymbolicPath) (cmmSources bi ++ (if ghcCompilesC then cSources bi ++ cxxSources bi ++ asmSources bi else [])))
                ghcSrcEntries = [(f, str f) | (f, _) <- ghcSrcs]
                perSrcFlags = [(f, strList opts) | (f, opts) <- ghcSrcs, not (null opts)]
                -- -I for the Haskell preprocessor (and for GHC's C
                -- compilation), as Cabal passes it.
                includeFlags = includeFlagsFor rootRelPkgDir bi
                cFlags = (if ghcCompilesC then map ("-optc" ++) (ccOptions bi) ++ map ("-optcxx" ++) (cxxOptions bi) else []) ++ includeFlags
            headers <- if ghcCompilesC then headerSources pkgDir bi else return []
            -- See Note [Configure build type]
            configured <- configureIncludes variant pkgDir targetName lbi bi
            let confLabel = autogenExportLabel variant rootRelPkgDir . fst <$> configured
                confFlags = ["-I$(location " ++ l ++ ")" | Just l <- [confLabel]]
            (cxxLoads, cxxDeps, cxxCalls) <-
              if ghcCompilesC then return ([], [], []) else cxxLibraryFor variant localIndex rootRelPkgDir pkgDir (targetName ++ "-cxx") (allDeps bi) confFlags bi
            macrosExport <- writeMacrosHeader variant pkgDir targetName pkgDesc lbi clbi
            let (pkgs, deps) = classifyDeps variant localIndex bi
                (unitArgs, hcFlags) = unitIdArgs lib clbi
                hlCall =
                  call
                    "haskell_library"
                    ( [ ("name", str targetName)
                      , ("srcs", VDict (srcs ++ ghcSrcEntries ++ [(h, str h) | h <- headers]))
                      ]
                        ++ cabalComponentArgs variant rootRelPkgDir targetName
                        ++ unitArgs
                        ++ compilerFlagsArg (hcFlags ++ projectGhcOptions lbi ++ cFlags ++ confFlags) bi
                        ++ optionalListArg "hsc_flags" (includeFlags ++ confFlags)
                        ++ [("per_src_flags", VDict perSrcFlags) | not (null perSrcFlags)]
                        ++ includeDirsArgs rootRelPkgDir confLabel bi
                        ++ reexportedModulesArg variant localIndex componentLBIs lbi clbi
                        ++ exportedLinkerFlagsArg bi
                        ++ optionalListArg "packages" pkgs
                        ++ optionalListArg "deps" (deps ++ cxxDeps)
                        ++ buildToolDependsArg variant localIndex externalBuildTools bi
                        ++ platformArg
                        ++ [("visibility", strList ["PUBLIC"])]
                    )
            return $
              PackageTargets
                (("//buck2:haskell.bzl", ["haskell_library"]) : cxxLoads)
                (cxxCalls ++ [hlCall])
                (macrosExport : [uncurry AutogenDir c | Just c <- [configured]] ++ srcAutogenExports)
      where
        noModules = null (exposedModules lib ++ otherModules (libBuildInfo lib))
        compiledSources bi = cmmSources bi ++ cSources bi ++ cxxSources bi ++ asmSources bi

    -- See Note [Packages without Haskell modules]
    nonHaskellLibrary targetName lib = case lbiClbiFor pkgDesc componentLBIs comp of
          Nothing -> skip ("library " ++ targetName ++ " (no LocalBuildInfo found for it in the elaborated build plan)")
          Just (lbi, clbi) -> do
            -- A header-only package is a registered unit without objects:
            -- GHC reads the unit's include-dirs (DerivedConstants.h of the
            -- rts unit), dependents get the headers as -I flags. A library
            -- that only re-exports modules (happy-lib) is registered the
            -- same way, with its re-exports.
            configured <- configureIncludes variant pkgDir targetName lbi bi
            let (unitArgs, _) = unitIdArgs lib clbi
                confLabel = autogenExportLabel variant rootRelPkgDir . fst <$> configured
            return $
              PackageTargets
                [("//buck2:haskell.bzl", ["haskell_library"])]
                [ call
                    "haskell_library"
                    ( [("name", str targetName), ("srcs", VDict [])]
                        ++ unitArgs
                        ++ includeDirsArgs rootRelPkgDir confLabel bi
                        ++ reexportedModulesArg variant localIndex componentLBIs lbi clbi
                        ++ exportedLinkerFlagsArg bi
                        ++ optionalListArg "deps" (allDeps bi)
                        ++ platformArg
                        ++ [("visibility", strList ["PUBLIC"])]
                    )
                ]
                [uncurry AutogenDir c | Just c <- [configured]]
      where
        bi = libBuildInfo lib

    -- The component's dependencies as buck2 labels: local libraries and
    -- third-party packages alike (a prebuilt package exports its C
    -- headers too, e.g. the boot compiler's rts).
    allDeps bi = let (pkgs, deps) = classifyDeps variant localIndex bi in deps ++ map (thirdPartyHaskellTargetLabel variant) pkgs

    executable exe
      | takeExtension mainIs `elem` [".c", ".cpp", ".cc", ".cxx"] && null (otherModules bi) = cExecutable
      | otherwise = case lbiClbiFor pkgDesc componentLBIs comp of
      Nothing -> skip ("executable " ++ targetName ++ " (no LocalBuildInfo found for it in the elaborated build plan)")
      Just (lbi, clbi) -> do
        mmainSrc <- resolveMainIs verbosity pkgDir bi mainIs
        motherSrcs <- resolveModules verbosity variant targetName pkgDesc (Just (lbi, clbi)) rootRelPkgDir pkgDir bi (otherModules bi)
        case (mmainSrc, motherSrcs) of
          (Just mainSrc0, Just (otherSrcs, srcAutogenExports)) -> do
            -- The C sources of an executable are compiled by GHC and
            -- linked as objects. See Note [C and Cmm sources].
            let ghcSrcs = sourcesWithOptions (map (normalise . getSymbolicPath) (cmmSources bi ++ cSources bi ++ cxxSources bi ++ asmSources bi))
                ghcSrcEntries = [(f, str f) | (f, _) <- ghcSrcs]
                perSrcFlags = [(f, strList opts) | (f, opts) <- ghcSrcs, not (null opts)]
                cFlags = map ("-optc" ++) (ccOptions bi) ++ map ("-optcxx" ++) (cxxOptions bi) ++ includeFlagsFor rootRelPkgDir bi
            macrosExport <- writeMacrosHeader variant pkgDir targetName pkgDesc lbi clbi
            let (pkgs, deps) = classifyDeps variant localIndex bi
                binCall =
                  call
                    "haskell_binary"
                    ( [ ("name", str targetName)
                      , ("srcs", VDict ((mainSrcKeyFor mainSrc0, str mainSrc0) : otherSrcs ++ ghcSrcEntries))
                      ]
                        ++ cabalComponentArgs variant rootRelPkgDir targetName
                        ++ compilerFlagsArg (hcOptions GHC bi ++ projectGhcOptions lbi ++ cFlags) bi
                        ++ [("per_src_flags", VDict perSrcFlags) | not (null perSrcFlags)]
                        ++ linkerFlagsArg (projectGhcOptions lbi) bi
                        ++ optionalListArg "packages" pkgs
                        ++ optionalListArg "deps" deps
                        ++ buildToolDependsArg variant localIndex externalBuildTools bi
                        ++ platformArg
                        ++ [("visibility", strList ["PUBLIC"])]
                    )
            return $
              PackageTargets
                [("//buck2:haskell.bzl", ["haskell_binary"])]
                [binCall]
                (macrosExport : srcAutogenExports)
          _ -> skip ("executable " ++ targetName ++ " (couldn't resolve all its modules)")
      where
        bi = componentBuildInfo (CExe exe)
        targetName = tname (unUnqualComponentName (exeName exe))
        mainIs = getSymbolicPath (modulePath exe)

        -- See Note [Packages without Haskell modules]
        cExecutable = do
          mmainSrc <- resolveMainIs verbosity pkgDir bi mainIs
          case mmainSrc of
            Nothing -> skip ("executable " ++ targetName ++ " (couldn't find its main-is file)")
            Just mainSrc -> do
              let (_pkgs, deps) = classifyDeps variant localIndex bi
                  srcs = normalise mainSrc : map getSymbolicPath (cSources bi ++ cxxSources bi)
                  binCall =
                    call
                      "cxx_binary"
                      ( [("name", str targetName), ("srcs", strList srcs)]
                          ++ optionalListArg "preprocessor_flags" (includeFlagsFor rootRelPkgDir bi)
                          ++ optionalListArg "compiler_flags" (ccOptions bi ++ cxxOptions bi)
                          ++ optionalListArg "linker_flags" (["-l" ++ lib | lib <- extraLibs bi] ++ ldOptions bi)
                          ++ optionalListArg "deps" deps
                          ++ platformArg
                          ++ [("visibility", strList ["PUBLIC"])]
                          ++ [("cxx_std", VBool False) | takeExtension mainIs == ".c", null (cxxSources bi)]
                      )
              return (PackageTargets [("//buck2:cxx.bzl", ["cxx_binary"])] [binCall] [])

    -- A benchmark's own 'BenchmarkExeV10' is exactly 'TestSuiteExeV10's
    -- shape (a version-tagged main-is path over the same 'BuildInfo') -
    -- and unlike a test-suite, @cabal bench@ has no special "run it and
    -- report a testsuite-style result" semantics of its own, just
    -- "build and run this executable" - so this reuses 'executable's
    -- plain @haskell_binary()@ mapping verbatim, with no @cwd@ wrapper
    -- (matching how a plain executable is already generated here).
    benchmark bench = case benchmarkInterface bench of
      BenchmarkExeV10 _ver mainIs -> case lbiClbiFor pkgDesc componentLBIs comp of
        Nothing -> skip ("benchmark " ++ targetName ++ " (no LocalBuildInfo found for it in the elaborated build plan)")
        Just (lbi, clbi) -> do
          mmainSrc <- resolveMainIs verbosity pkgDir bi (getSymbolicPath mainIs)
          motherSrcs <- resolveModules verbosity variant targetName pkgDesc (Just (lbi, clbi)) rootRelPkgDir pkgDir bi (otherModules bi)
          case (mmainSrc, motherSrcs) of
            (Just mainSrc0, Just (otherSrcs, srcAutogenExports)) -> do
              (cxxLoads, cxxDeps, cxxCalls) <- cxxLibraryFor variant localIndex rootRelPkgDir pkgDir (targetName ++ "-cxx") (allDeps bi) [] bi
              macrosExport <- writeMacrosHeader variant pkgDir targetName pkgDesc lbi clbi
              let (pkgs, deps) = classifyDeps variant localIndex bi
                  binCall =
                    call
                      "haskell_binary"
                      ( [ ("name", str targetName)
                        , ("srcs", VDict ((mainSrcKeyFor mainSrc0, str mainSrc0) : otherSrcs))
                        ]
                          ++ cabalComponentArgs variant rootRelPkgDir targetName
                          ++ compilerFlagsArg (hcOptions GHC bi ++ projectGhcOptions lbi) bi
                          ++ linkerFlagsArg (projectGhcOptions lbi) bi
                          ++ optionalListArg "packages" pkgs
                          ++ optionalListArg "deps" (deps ++ cxxDeps)
                          ++ buildToolDependsArg variant localIndex externalBuildTools bi
                          ++ platformArg
                          ++ [("visibility", strList ["PUBLIC"])]
                      )
              return $
                PackageTargets
                  (("//buck2:haskell.bzl", ["haskell_binary"]) : cxxLoads)
                  (cxxCalls ++ [binCall])
                  (macrosExport : srcAutogenExports)
            _ -> skip ("benchmark " ++ targetName ++ " (couldn't resolve all its modules)")
      _ ->
        skip
          ( "benchmark "
              ++ targetName
              ++ " (only exitcode-stdio-1.0 benchmarks are supported)"
          )
      where
        bi = componentBuildInfo (CBench bench)
        targetName = tname (unUnqualComponentName (benchmarkName bench))

    testSuite test = case testInterface test of
      TestSuiteExeV10 _ver mainIs -> case lbiClbiFor pkgDesc componentLBIs comp of
        Nothing -> skip ("test-suite " ++ targetName ++ " (no LocalBuildInfo found for it in the elaborated build plan)")
        Just (lbi, clbi) -> do
          mmainSrc <- resolveMainIs verbosity pkgDir bi (getSymbolicPath mainIs)
          motherSrcs <- resolveModules verbosity variant targetName pkgDesc (Just (lbi, clbi)) rootRelPkgDir pkgDir bi (otherModules bi)
          case (mmainSrc, motherSrcs) of
            (Just mainSrc0, Just (otherSrcs, srcAutogenExports)) ->
              mkTestCall lbi clbi (mainSrcKeyFor mainSrc0) (str mainSrc0) otherSrcs srcAutogenExports
            _ -> skip ("test-suite " ++ targetName ++ " (couldn't resolve all its modules)")
      -- A @detailed-0.9@ test-suite's own module (named via
      -- @test-module:@, not @other-modules:@ - real Cabal synthesises a
      -- whole separate internal sub-library exposing just this one
      -- module, see 'Distribution.Simple.Build.testSuiteLibV09AsLibAndExe')
      -- exports @tests :: IO ['Distribution.TestSuite.Test']@, and real
      -- Cabal's own Setup.hs generates a tiny stub 'Main' importing it
      -- and calling into 'Distribution.Simple.Test.LibV09.stubMain' -
      -- which then blocks reading a @(logFilePath, testSuiteName)@ pair
      -- off *stdin*, written by the parent @cabal test@ process, before
      -- it'll run anything at all (see that module's own 'stubMain').
      -- That stdin handshake has nothing to do with buck2 - a
      -- @haskell_test()@ just execs the compiled binary and checks its
      -- exit code, the same as @exitcode-stdio-1.0@ - so reusing real
      -- Cabal's own stub verbatim would need a wrapper script to feed it
      -- a fake handshake for no real benefit (nothing here ever reads
      -- the machine-readable log it writes). Generates a self-contained
      -- stub instead, calling only 'Distribution.TestSuite's own public
      -- API directly (a real, documented, GHC-version-independent
      -- interface - not reimplementing anything real Cabal doesn't
      -- already expose for exactly this purpose): runs every 'Test'
      -- in turn, printing a human-readable pass\/fail\/error line per
      -- test to stdout, exiting non-zero if anything failed or errored.
      -- Needs no new build-depends beyond what real Cabal already
      -- requires the user to declare for their own @tests@ module to
      -- even type-check (@Distribution.TestSuite@ lives in the @Cabal@
      -- library itself).
      TestSuiteLibV09 _ver testModule -> case lbiClbiFor pkgDesc componentLBIs comp of
        Nothing -> skip ("test-suite " ++ targetName ++ " (no LocalBuildInfo found for it in the elaborated build plan)")
        Just (lbi, clbi) -> do
          mtestModSrc <- resolveModules verbosity variant targetName pkgDesc (Just (lbi, clbi)) rootRelPkgDir pkgDir bi [testModule]
          motherSrcs <- resolveModules verbosity variant targetName pkgDesc (Just (lbi, clbi)) rootRelPkgDir pkgDir bi (otherModules bi)
          case (mtestModSrc, motherSrcs) of
            (Just (testModSrc, testModAutogenExports), Just (otherSrcs, otherAutogenExports)) -> do
              (stubLabel, stubAutogenExport) <- writeDetailedTestStub variant rootRelPkgDir pkgDir targetName testModule
              mkTestCall
                lbi
                clbi
                "Main.hs"
                (str stubLabel)
                (testModSrc ++ otherSrcs)
                (stubAutogenExport : testModAutogenExports ++ otherAutogenExports)
            _ -> skip ("test-suite " ++ targetName ++ " (couldn't resolve all its modules)")
      _ ->
        skip
          ( "test-suite "
              ++ targetName
              ++ " (only exitcode-stdio-1.0 and detailed-0.9 test-suites are supported)"
          )
      where
        bi = componentBuildInfo (CTest test)
        targetName = tname (unUnqualComponentName (testName test))
        mkTestCall lbi clbi mainSrcKey mainSrc otherSrcs srcAutogenExports = do
          (cxxLoads, cxxDeps, cxxCalls) <- cxxLibraryFor variant localIndex rootRelPkgDir pkgDir (targetName ++ "-cxx") (allDeps bi) [] bi
          macrosExport <- writeMacrosHeader variant pkgDir targetName pkgDesc lbi clbi
          let (pkgs, deps) = classifyDeps variant localIndex bi
              testCall =
                call
                  "haskell_test"
                  ( [ ("name", str targetName)
                    , ("srcs", VDict ((mainSrcKey, mainSrc) : otherSrcs))
                    , -- Real `cabal test` always runs a test-suite with its
                      -- cwd set to the package's own directory - matched
                      -- here so a test that reads its own fixture files by
                      -- a package-relative path (extremely common) works
                      -- the same way under buck2 (see haskell_test()'s own
                      -- haddock in buck2/haskell.bzl for why this needs a
                      -- generated wrapper, not just a plain attr).
                      ("cwd", str rootRelPkgDir)
                    ]
                      ++ cabalComponentArgs variant rootRelPkgDir targetName
                      ++ compilerFlagsArg (hcOptions GHC bi ++ projectGhcOptions lbi) bi
                      ++ linkerFlagsArg (projectGhcOptions lbi) bi
                      ++ optionalListArg "packages" pkgs
                      ++ optionalListArg "deps" (deps ++ cxxDeps)
                      ++ buildToolDependsArg variant localIndex externalBuildTools bi
                      ++ platformArg
                  )
          return $
            PackageTargets
              (("//buck2:haskell.bzl", ["haskell_test"]) : cxxLoads)
              (cxxCalls ++ [testCall])
              (macrosExport : srcAutogenExports)

-- | @ghc-options@ + @cpp-options@ + @default-extensions@ (as @-X...@
-- flags), the sources of per-component GHC flags buck2's @compiler_flags@
-- covers. @other-extensions@ is deliberately excluded: those are declared
-- via in-module @LANGUAGE@ pragmas, not enabled component-wide. The
-- @cabal_macros.h@ @-optP-include@ flags are *not* here any more - they're
-- injected by buck2\/haskell.bzl's own @cabal_component@ kwarg (see
-- 'cabalComponentArg'), which - unlike a plain string folded into this
-- list - can be a real, buck2-tracked dependency edge.
compilerFlagsArg :: [String] -> BuildInfo -> [(String, Value)]
compilerFlagsArg ghcFlags bi =
  optionalListArg "compiler_flags" (ghcFlags ++ cppOptions bi ++ languageFlag ++ extensionFlags)
  where
    -- default-language isn't just documentation: GHC2021/GHC2024 each
    -- imply a large bundle of extensions (TypeApplications among them) -
    -- omitting this made GHC silently fall back to its own default
    -- (Haskell2010) instead, which is how Cabal-syntax's actual use of
    -- TypeApplications (implied by its own `default-language: GHC2021`,
    -- with nothing naming TypeApplications directly) went unnoticed
    -- until a real `buck2 build` on it failed outright.
    languageFlag = ["-X" ++ prettyShow lang | lang <- maybeToList (defaultLanguage bi)]
    -- usedExtensions: default-extensions and the legacy extensions field
    extensionFlags = ["-X" ++ prettyShow ext | ext <- usedExtensions bi]

-- | @extra-libraries@ on a library, as @-l@ flags on its
-- @exported_linker_flags@
exportedLinkerFlagsArg :: BuildInfo -> [(String, Value)]
exportedLinkerFlagsArg bi = optionalListArg "exported_linker_flags" ["-l" ++ lib | lib <- extraLibs bi]

-- | @extra-libraries@ (as @-l@ flags) plus @ghc-options@ on an
-- executable\/test-suite's plain @linker_flags@ - correct as-is here
-- (unlike on a library): both rules already apply @linker_flags@ directly
-- to their own, one and only, final executable link. @ghc-options@ is
-- included here too, not just in @compiler_flags@: buck2's haskell_binary
-- only passes @compiler_flags@ to each module's own compile step, not the
-- final link (a real @ghc -o@ invocation, distinct from that), whereas
-- Cabal applies a component's whole @ghc-options@ to every ghc invocation
-- for it, compile and link alike - so anything there that's actually
-- link-relevant (e.g. @-threaded@\/@-rtsopts@, which select the RTS
-- linked in) needs to reach buck2's link step explicitly via
-- @linker_flags@, or it's silently dropped. A compile-only flag showing
-- up here too (e.g. @-Wall@) is harmless: GHC's link-mode invocation
-- just ignores flags that don't apply to it.
linkerFlagsArg :: [String] -> BuildInfo -> [(String, Value)]
linkerFlagsArg extraGhcFlags bi = optionalListArg "linker_flags" (["-l" ++ lib | lib <- extraLibs bi] ++ hcOptions GHC bi ++ extraGhcFlags)

-- Note [Project-level ghc-options]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A cabal.project can add GHC flags to a package (@package rts
-- ghc-options: -no-rts@), to all packages (@program-options@), or from the
-- command line (@--ghc-options@). Cabal passes them to every GHC call for
-- the component, after the .cabal file's own ghc-options. They are in the
-- component's configured 'LocalBuildInfo' as the override arguments of
-- the ghc program, which is where 'projectGhcOptions' takes them from.

-- | Removes @-this-unit-id X@ from GHC flags and returns @X@ (the last
-- one, as GHC itself takes it). See 'unitIdArgs'.
splitThisUnitId :: [String] -> ([String], Maybe String)
splitThisUnitId = go Nothing
  where
    go _ ("-this-unit-id" : uid : rest) = go (Just uid) rest
    go acc (f : rest)
      | Just uid <- stripPrefix "-this-unit-id=" f = go (Just uid) rest
      | otherwise = let (fs, acc') = go acc rest in (f : fs, acc')
    go acc [] = ([], acc)

-- | Writes this component's @cabal_macros.h@ to a real file next to the
-- package's own sources - exactly how real Cabal wires this up, just
-- generated ahead of time instead of by Setup.hs at configure time.
-- Written unconditionally, like real Cabal: harmless for a component
-- that never enables CPP (the header only matters if\/when cpp actually
-- runs). Returns the @cabal-buck2\/autogen\/BUCK@ export entry for it
-- (see 'PackageTargets'' own haddock) - 'cabalComponentArg' is what
-- actually wires the generated component up to include it.
writeMacrosHeader :: Variant -> FilePath -> String -> PackageDescription -> LocalBuildInfo -> ComponentLocalBuildInfo -> IO AutogenExport
writeMacrosHeader variant pkgDir targetName pkgDesc lbi clbi = do
  createDirectoryIfMissing True headerDir
  writeFile headerPath (generateCabalMacrosHeader pkgDesc lbi clbi)
  return (AutogenFile (targetName ++ "-cabal-macros") exportRelPath)
  where
    exportRelPath = targetName </> "cabal_macros.h"
    headerDir = pkgDir </> variantAutogenDir variant </> targetName
    headerPath = headerDir </> "cabal_macros.h"

-- | The @cabal_component = (pkg, component)@ kwarg understood by
-- buck2\/haskell.bzl's @haskell_library()@\/@haskell_binary()@ (and, via
-- that, @haskell_test()@ - see its own comment there): tells the rule
-- which @cabal-buck2\/autogen\/BUCK@ export target holds *this*
-- component's own @cabal_macros.h@, so it can inject
-- @-optP-include -optP$(location ...)@ itself, as a real dependency edge
-- (unlike a plain path string folded into @compiler_flags@, which isn't
-- buck2-tracked at all). @pkg@ must
-- match 'localTargetLabel''s own directory convention (@.@ at the
-- project root) - haskell.bzl computes the matching @export_file()@
-- label (@\/\/pkg\/cabal-buck2\/autogen:component-cabal-macros@) the
-- same way "Distribution.Client.Buck2.Generate" lays out
-- @cabal-buck2\/autogen\/BUCK@ itself.
cabalComponentArgs :: Variant -> FilePath -> String -> [(String, Value)]
cabalComponentArgs variant rootRelPkgDir targetName =
  ("cabal_component", VTuple [str rootRelPkgDir, str targetName])
    : [("cabal_autogen_dir", str (variantAutogenDir variant)) | isJust (variantName variant)]

optionalListArg :: String -> [String] -> [(String, Value)]
optionalListArg _ [] = []
optionalListArg name xs = [(name, strList (ordNub xs))]

-- | The buck2 target name for one of a package's libraries: the package
-- name itself for the main (unnamed) library, matching every other
-- reference to it (@packages = [...]@, other packages' @build-depends@,
-- ...); the sub-library's own unqualified name otherwise - always unique
-- within one package's BUCK file, since Cabal itself already requires
-- every component name in a package to be distinct.
libTargetName :: PackageName -> LibraryName -> String
libTargetName pn LMainLibName = unPackageName pn
libTargetName _ (LSubLibName n) = unUnqualComponentName n

-- | Split a component's @build-depends@ (each of which may name one or
-- more specific sub-libraries of a package via @pkg:sublib@ - see
-- 'depLibraries') into external *main*-library package names (fed to
-- buck2/haskell.bzl's @packages =@ convenience param - which only ever
-- resolves a package's main library, per its own haddock in
-- buck2/haskell.bzl) and target labels (fed to @deps =@): a local
-- package's own sub-library target (@//dir:sublib@, via 'libTargetName')
-- when one was named, an *external* package's own named sub-library
-- target (@//third-party/haskell:sublib@ - "Distribution.Client.
-- Buck2.Prebuilt" generates one @haskell_prebuilt_library()@ per library
-- unit there too, target-named the exact same way via the same
-- 'libTargetName', not one per package name) when one was named there
-- instead, or nothing at all for an ordinary external main-library
-- dependency (that one's covered by @packages =@ already).
-- Note [Re-exported modules]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A library can re-export modules of its dependencies (GHC's ghc-prim
-- re-exports GHC.Internal.Prim of ghc-internal as GHC.Prim). Cabal
-- registers them in @exposed-modules@ as @New from <unit-id>:Orig@, with
-- the unit id of the dependency as configured. The registered id of a
-- buck2 library can differ from the configured one (@-this-unit-id@ in
-- its ghc-options), so the generated rule names the dependency by its
-- label and the prelude fills in the registered id:
--
--   reexported_modules = {'GHC.Prim': ('//libraries/ghc-internal:ghc-internal-stage2', 'GHC.Internal.Prim')}
--
-- A module re-exported from the library itself (@Paths_x as X.Paths@)
-- has @None@ for the label. Cabal resolves a chain of re-exports to the
-- unit that defines the module, which need not be a direct dependency
-- (GHC's compiler re-exports GHC.Platform.ArchOS of ghc-platform through
-- ghc-boot), so the unit is looked up among all local libraries and the
-- installed packages.

-- | The @reexported_modules@ kwarg of a library (see Note [Re-exported
-- modules]), from the exposed modules Cabal resolved at configure time.
reexportedModulesArg
  :: Variant
  -> LocalPackageIndex
  -> Map (PackageName, ComponentName) LocalBuildInfo
  -> LocalBuildInfo
  -> ComponentLocalBuildInfo
  -> [(String, Value)]
reexportedModulesArg variant localIndex componentLBIs lbi clbi =
  [("reexported_modules", VDict entries) | not (null entries)]
  where
    entries =
      [ (prettyShow (exposedName em), VTuple [origin (unDefUnitId uid), str (prettyShow m)])
      | em <- fromMaybe [] (maybeComponentExposedModules clbi)
      , Just (OpenModule (DefiniteUnitId uid) m) <- [exposedReexport em]
      ]
    origin uid
      | uid == componentUnitId clbi = VNone
      | Just (pn, ln) <- Map.lookup uid local_units = str (libraryLabel variant localIndex pn ln)
      | Just ipi <- PackageIndex.lookupUnitId (installedPkgs lbi) uid = str (libraryLabel variant localIndex (packageName ipi) (sourceLibName ipi))
      | otherwise = VNone
    -- The configured unit ids of the local libraries.
    local_units =
      Map.fromList
        [ (componentUnitId c, (pn, ln))
        | ((pn, cname@(CLibName ln)), l) <- Map.toList componentLBIs
        , c <- componentNameCLBIs l cname
        ]

-- | The label of a library a component depends on: a local package's
-- target, or a target of the third-party cell.
libraryLabel :: Variant -> LocalPackageIndex -> PackageName -> LibraryName -> String
libraryLabel variant localIndex pn ln = case Map.lookup pn localIndex of
  Just (dir, _) -> localTargetLabel dir (variantTargetName variant (libTargetName pn ln))
  Nothing -> thirdPartyHaskellTargetLabel variant (libTargetName pn ln)

classifyDeps :: Variant -> LocalPackageIndex -> BuildInfo -> ([String], [String])
classifyDeps variant localIndex bi
  -- In a variant, external packages are labels of the variant's own
  -- third-party cell, in `deps` (buck2/haskell.bzl's `packages` kwarg
  -- names targets of the base cell). See Note [Variants].
  | isJust (variantName variant) = ([], thirdParty externalMain ++ externalSubs ++ localLabels)
  | otherwise = (externalMain, externalSubs ++ localLabels)
  where
    thirdParty = map (thirdPartyHaskellTargetLabel variant)
    externalMain = ordNub [unPackageName pn | (pn, LMainLibName) <- allDeps, not (Map.member pn localIndex)]
    externalSubs = thirdParty (ordNub [libTargetName pn ln | (pn, ln@(LSubLibName _)) <- allDeps, not (Map.member pn localIndex)])
    localLabels = ordNub [localTargetLabel dir (variantTargetName variant (libTargetName pn ln)) | (pn, ln) <- allDeps, Just (dir, _) <- [Map.lookup pn localIndex]]
    directDeps =
      ordNub
        [ (depPkgName d, ln)
        | d <- targetBuildDepends bi
        , ln <- NES.toList (depLibraries d)
        ]
    allDeps = closeOverReexports [] directDeps
    closeOverReexports seen [] = seen
    closeOverReexports seen (p@(pn, _) : rest)
      | p `elem` seen = closeOverReexports seen rest
      | otherwise =
          let origins = maybe [] snd (Map.lookup pn localIndex)
           in closeOverReexports (p : seen) (rest ++ [(o, LMainLibName) | o <- origins])

-- | A target of the variant's third-party cell: @third-party-haskell//:x@,
-- or @third-party-haskell-stage2//:x@ (a cell alias of .buckconfig for
-- the variant's @third-party/haskell-stage2@ directory). See Note [Variants].
thirdPartyHaskellTargetLabel :: Variant -> String -> String
thirdPartyHaskellTargetLabel variant name = "third-party-haskell" ++ variantSuffix variant ++ "//:" ++ name

localTargetLabel :: FilePath -> String -> String
localTargetLabel dir targetName = "//" ++ (if dir == "." then "" else dir) ++ ":" ++ targetName

-- | @build_tool_depends = [...]@ - one real buck2 target label per
-- @build-tool-depends:@ executable this component declares, letting
-- buck2\/prelude\/haskell\/compile.bzl's own @compile()@ put each one on
-- @PATH@ for exactly this rule's own compile actions (see that attr's
-- own haddock in @buck2\/prelude\/decls\/haskell_common.bzl@) - not a
-- single project-wide directory shared by every component.
--
-- A build-tool-depends naming a *local* package's own executable
-- component references that component's real, already-generated
-- @haskell_binary()@ target directly (the exact same label 'executable'
-- itself produces for it: @unUnqualComponentName exe@ is, by
-- construction, that function's own @targetName@ too) - buck2 then
-- builds it as a genuine dependency, unlike the old host-filesystem-
-- symlink approach, which only ever worked for already-installed
-- *external* dependencies (a local one doesn't exist on disk at all
-- until buck2 itself builds it). An external dependency instead
-- references the @export_file()@-wrapped target
-- "Distribution.Client.Buck2.Prebuilt" generates for it (see
-- 'generatePrebuilt' - @externalBuildTools@ here is exactly the set of
-- names it resolved a real binary for and generated a target for).
--
-- An entry naming a tool that's neither a local package's own
-- executable nor a resolved external one is silently omitted - most
-- @build-tool-depends@ executables are ordinary Setup.hs-time tools
-- with nothing to do with GHC needing them on @PATH@, so a miss here is
-- the overwhelmingly common, unremarkable case (the same reasoning
-- "Distribution.Client.Buck2.Prebuilt"'s own resolution step already
-- used); a real @buck2 build@ still fails exactly the way it always did
-- if the omission actually mattered (a bare tool name GHC can't find).
--
-- Known gap, shared with 'classifyDeps's own @deps@ list: if the local
-- executable a build-tool-depends names is itself skipped (see
-- 'skippedLibraries's own reasoning, which only covers *libraries*),
-- this would reference a target that was never generated - unlike the
-- library case, not currently guarded against. Not a regression versus
-- the old design (which could never reference a local build-tool-depends
-- executable at all, skipped or not - the bug this rewrite exists to
-- fix), so left as a known, pre-existing class of limitation rather than
-- solved here.
buildToolDependsArg :: Variant -> LocalPackageIndex -> Set String -> BuildInfo -> [(String, Value)]
buildToolDependsArg variant localIndex externalBuildTools bi =
  optionalListArg "build_tool_depends" (mapMaybe resolve (ordNub [(pn, exe) | ExeDependency pn exe _ <- buildToolDepends bi]))
  where
    -- Tools are exec deps of the base variant: an unsuffixed local target,
    -- or the base variant's third-party tool. See Note [Variants].
    resolve (pn, exe)
      | exeName `Set.member` variantBaseTools variant = Just (thirdPartyHaskellTargetLabel baseVariant (exeName ++ "-exe"))
      | otherwise = case Map.lookup pn localIndex of
          Just (dir, _) -> Just (localTargetLabel dir exeName)
          Nothing
            | exeName `Set.member` externalBuildTools ->
                Just (thirdPartyHaskellTargetLabel variant (exeName ++ "-exe"))
            | otherwise -> Nothing
      where
        exeName = unUnqualComponentName exe

-- | The target label for one @cabal-buck2\/autogen\/BUCK@ export (see
-- 'PackageTargets') - needed (not just the plain file path) to reference
-- an autogen file from @srcs@ once it has its own @export_file()@ entry:
-- @cabal-buck2\/autogen\/@ is a *different* buck2 package from @pkgDir@
-- itself (having its own @BUCK@ file is what makes a directory a
-- package), so a file living there is no longer a plain same-package
-- source path as far as @pkgDir@'s own rules are concerned, even though
-- it's still on disk right where it always was.
autogenExportLabel :: Variant -> FilePath -> String -> String
autogenExportLabel variant rootRelPkgDir = localTargetLabel autogenDir
  where
    autogenDir
      | rootRelPkgDir == "." = variantAutogenDir variant
      | otherwise = rootRelPkgDir </> variantAutogenDir variant

-- | Resolve each module in @hs-source-dirs@ to its real file, trying
-- @.hs@\/@.lhs@\/@.hsc@ in turn (the extensions buck2/hsc2hs.bzl knows how
-- to handle) - returning @(moduleDerivedPath, realRelativePath)@ pairs for
-- the dict form of @srcs@, which - unlike the plain-list form - is
-- unaffected by @hs-source-dirs@ not matching the BUCK package's own
-- directory. The package's @Paths_<pkg>@ autogen module (if listed) is
-- special-cased: no such file exists anywhere - Cabal's own Setup.hs
-- generates it fresh on every real build - so 'writePathsModule' stands
-- one in ourselves rather than searching for it, which needs this
-- component's real 'LocalBuildInfo'\/'ComponentLocalBuildInfo' (see
-- 'lbiClbiFor') the same way 'writeMacrosHeader' does; 'Nothing' here (no
-- LBI available for this component) fails this module's own resolution,
-- same as a missing source file would. Also returns every
-- @cabal-buck2\/autogen\/BUCK@ export entry (see 'PackageTargets') picked
-- up along the way - in practice just the @Paths_\<pkg\>@ one, at most
-- once, if that module was among @mods@.
--
-- 'Nothing' if *any* module couldn't be resolved - the caller skips the
-- whole component in that case, rather than emitting a rule that
-- references a source file that doesn't exist: buck2 doesn't merely warn
-- about that, it fails outright while evaluating the @BUCK@ file, which
-- (since a @buck2 build //...@ evaluates every @BUCK@ file up front)
-- would otherwise take the *entire* build down over one unresolvable
-- module in one component of one package.
resolveModules :: Verbosity -> Variant -> String -> PackageDescription -> Maybe (LocalBuildInfo, ComponentLocalBuildInfo) -> FilePath -> FilePath -> BuildInfo -> [ModuleName.ModuleName] -> IO (Maybe ([(String, Value)], [AutogenExport]))
resolveModules verbosity variant targetName pkgDesc mlbiClbi rootRelPkgDir pkgDir bi mods = do
  results <- traverse (resolveOne verbosity variant targetName pkgDesc mlbiClbi rootRelPkgDir pkgDir (sourceDirs bi) (autogenModules bi)) mods
  return $ case sequenceA results of
    Nothing -> Nothing
    Just pairs -> Just (concatMap fst pairs, concatMap snd pairs)

-- Note [Autogen modules without a generator]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A module listed in @autogen-modules@ has no source file in the tree.
-- Cabal itself generates @Paths_pkg@ and @PackageInfo_pkg@, and we do the
-- same here. Any other autogen module is produced by the package's own
-- Setup.hs (build-type Custom), which @cabal buck2@ does not run. For
-- example GHC's @compiler@ package generates @GHC.Platform.Constants@ with
-- the @deriveConstants@ tool in its Setup.hs.
--
-- Instead of skipping the whole component, we map such a module to the
-- same-package target @:<target>-autogen-<Module.Name>@:
--
--   srcs = { 'GHC/Platform/Constants.hs': ':ghc-autogen-GHC.Platform.Constants' }
--
-- The hand-maintained @BUCK@ file must define that target (a @genrule@ or
-- an @export_file@) with the module source as its output. buck2 reports
-- an unknown target if it is missing, and the warning we emit here names
-- the target to define.
resolveOne :: Verbosity -> Variant -> String -> PackageDescription -> Maybe (LocalBuildInfo, ComponentLocalBuildInfo) -> FilePath -> FilePath -> [FilePath] -> [ModuleName.ModuleName] -> ModuleName.ModuleName -> IO (Maybe ([(String, Value)], [AutogenExport]))
resolveOne verbosity variant targetName pkgDesc mlbiClbi rootRelPkgDir pkgDir dirs autogens m
  | m == autogenPackageInfoModuleName pkgDesc = do
      (label, autogenExport) <- writePackageInfoModule variant rootRelPkgDir pkgDir pkgDesc m
      return (Just ([(ModuleName.toFilePath m <.> "hs", str label)], [autogenExport]))
  | m == autogenPathsModuleName pkgDesc = case mlbiClbi of
      Nothing -> do
        warn verbosity $
          "cabal buck2: couldn't generate " ++ prettyShow m ++ " (no LocalBuildInfo available for this component)"
        return Nothing
      Just (lbi, clbi) -> do
        (label, autogenExport) <- writePathsModule variant rootRelPkgDir pkgDir pkgDesc lbi clbi m
        return (Just ([(ModuleName.toFilePath m <.> "hs", str label)], [autogenExport]))
  | otherwise = do
      let modPath = ModuleName.toFilePath m
          hsPath = modPath <.> "hs"
      -- A configure script (build-type Configure) can generate a module
      -- into the build directory, which Cabal searches before the source
      -- directories (GHC's ghc-internal: GHC.Internal.Prim, with a stub
      -- in src/ too). Such a module is treated like an autogen module
      -- without a generator.
      inBuildDir <- case mlbiClbi of
        Just (lbi, _) -> doesFileExist (interpretSymbolicPath (mbWorkDirLBI lbi) (buildDir lbi) </> hsPath)
        Nothing -> return False
      -- buck2/haskell.bzl's own srcs-resolution (_resolve_src) auto-detects
      -- .hsc/.x/.y by the *source* file's extension and runs it through
      -- hsc2hs()/alex()/happy() - already loaded by haskell.bzl itself, so
      -- nothing extra needs to be loaded here for that to work.
      found <-
        if inBuildDir
          then return Nothing
          else firstExistingIn pkgDir dirs [modPath <.> ext | ext <- ["hs", "lhs", "hsc", "x", "y"]]
      case found of
        Just (dir, file) -> do
          -- A boot file next to the module goes into srcs too: the prelude
          -- passes it to GHC as a hidden input (it is not a compiler
          -- argument, but GHC reads it from the source directory).
          boots <- filterM (\b -> doesFileExist (pkgDir </> dir </> b)) [modPath <.> "hs-boot", modPath <.> "lhs-boot"]
          outside <- outsideSourceLabel (pkgDirToRoot rootRelPkgDir pkgDir) rootRelPkgDir dir
          let -- Normalised so that a source at its module path is used in
              -- place ("./X.hs" would make buck2/haskell.bzl copy it).
              entry f = (f, str (fromMaybe (normalise (dir </> f)) (outside f)))
          for_ (outside file) $ \label ->
            -- See Note [Sources outside the package directory]
            warn verbosity $
              "cabal buck2: module "
                ++ prettyShow m
                ++ " lives outside the package directory ("
                ++ dir
                ++ "); the target "
                ++ label
                ++ " must export it"
          -- A literate source keeps its extension: GHC unlits it.
          let key = modPath <.> (if takeExtension file == ".lhs" then "lhs" else "hs")
          return (Just ((key, snd (entry file)) : map entry boots, []))
        Nothing ->
          if m `elem` autogens || inBuildDir
            then do
              -- See Note [Autogen modules without a generator]
              warn verbosity $
                "cabal buck2: "
                  ++ (if inBuildDir then "module " ++ prettyShow m ++ " is generated into the build directory" else "autogen module " ++ prettyShow m ++ " has no source file")
                  ++ "; the target :"
                  ++ autogenTargetName targetName m
                  ++ " in "
                  ++ (rootRelPkgDir </> "BUCK")
                  ++ " must provide it"
              return (Just ([(hsPath, str (":" ++ autogenTargetName targetName m))], []))
            else do
              warn verbosity $
                "cabal buck2: couldn't find a source file for module "
                  ++ prettyShow m
                  ++ " under "
                  ++ intercalate ", " dirs
              return Nothing

-- | Name of the hand-written target that provides an autogen module of a
-- component: @<target>-autogen-<Module.Name>@, so that two variants of the
-- component (see Note [Variants]) can have different generators.
-- See Note [Autogen modules without a generator].
autogenTargetName :: String -> ModuleName.ModuleName -> String
autogenTargetName targetName m = targetName ++ "-autogen-" ++ prettyShow m

-- | Writes @PackageInfo_<pkg>.hs@ under @cabal-buck2\/autogen@, with the
-- same content real Cabal generates. Like 'writePathsModule', but it needs
-- no 'LocalBuildInfo'.
writePackageInfoModule :: Variant -> FilePath -> FilePath -> PackageDescription -> ModuleName.ModuleName -> IO (String, AutogenExport)
writePackageInfoModule variant rootRelPkgDir pkgDir pkgDesc m = do
  createDirectoryIfMissing True (pkgDir </> variantAutogenDir variant)
  writeFile (pkgDir </> relPath) (generatePackageInfoModule pkgDesc)
  return (autogenExportLabel variant rootRelPkgDir exportName, AutogenFile exportName moduleFileName)
  where
    exportName = ModuleName.toFilePath m
    moduleFileName = exportName <.> "hs"
    relPath = variantAutogenDir variant </> moduleFileName

-- | Cabal's own Setup.hs generates a @Paths_\<pkg\>@ module fresh at
-- configure\/build time (giving @version@\/@getDataFileName@\/etc) - no
-- real source file for it exists anywhere to find. Written here from
-- real Cabal's own 'generatePathsModule' (given this component's real
-- 'LocalBuildInfo'\/'ComponentLocalBuildInfo' - see 'lbiClbiFor'), so
-- install-dir\/relocatability logic matches a plain @cabal build@
-- exactly, instead of the hand-rolled @return "."@ stand-in this used to
-- be before a real 'LocalBuildInfo' was available here. Also returns its
-- own @cabal-buck2\/autogen\/BUCK@ export entry (see 'PackageTargets') -
-- written afresh, and so exported afresh, every time a component
-- happens to reference @Paths_\<pkg\>@, even though it's the same file
-- each time; 'PackageTargets''s own 'Semigroup' instance dedupes the
-- repeats away.
--
-- The @String@ returned for the caller's own @srcs@ entry is the
-- @export_file()@ target's label ('autogenExportLabel'), *not* a plain
-- file path: @cabal-buck2\/autogen\/@ has its own @BUCK@ file (written
-- by "Distribution.Client.Buck2.Generate"), so it's a different buck2
-- package from @pkgDir@ - a file living there can no longer be named by
-- a same-package-relative path from @pkgDir@'s own rules, only by a real
-- target reference (which @attrs.source()@, @srcs@'s own element type,
-- accepts just as well as a path).
writePathsModule :: Variant -> FilePath -> FilePath -> PackageDescription -> LocalBuildInfo -> ComponentLocalBuildInfo -> ModuleName.ModuleName -> IO (String, AutogenExport)
writePathsModule variant rootRelPkgDir pkgDir pkgDesc lbi clbi m = do
  createDirectoryIfMissing True (pkgDir </> variantAutogenDir variant)
  writeFile (pkgDir </> relPath) (generatePathsModule pkgDesc lbi clbi)
  return (autogenExportLabel variant rootRelPkgDir exportName, AutogenFile exportName moduleFileName)
  where
    exportName = ModuleName.toFilePath m
    moduleFileName = exportName <.> "hs"
    relPath = variantAutogenDir variant </> moduleFileName

-- | A @detailed-0.9@ test-suite's own stub @Main@ - see 'testSuite's own
-- haddock for why this is a from-scratch driver over
-- @Distribution.TestSuite@'s public API, not real Cabal's own
-- @Setup.hs@-generated one (@Distribution.Simple.Test.LibV09.stubMain@,
-- which expects a handshake over stdin buck2 has no way to provide).
-- Returns its own @srcs@-entry label and @cabal-buck2\/autogen\/BUCK@
-- export entry the same way 'writePathsModule' does, and for the same
-- reason (a different buck2 package from @pkgDir@ once
-- @cabal-buck2\/autogen\/BUCK@ exists).
writeDetailedTestStub :: Variant -> FilePath -> FilePath -> String -> ModuleName.ModuleName -> IO (String, AutogenExport)
writeDetailedTestStub variant rootRelPkgDir pkgDir targetName testModule = do
  createDirectoryIfMissing True (pkgDir </> dir)
  writeFile (pkgDir </> relPath) contents
  return (autogenExportLabel variant rootRelPkgDir exportName, AutogenFile exportName exportRelPath)
  where
    exportName = targetName ++ "-stub-main"
    exportRelPath = targetName </> "Main.hs"
    dir = variantAutogenDir variant </> targetName
    relPath = dir </> "Main.hs"
    contents =
      unlines
        [ "-- @generated by `cabal buck2` - do not edit by hand."
        , "module Main (main) where"
        , ""
        , "import Distribution.TestSuite"
        , "import qualified " ++ prettyShow testModule ++ " as CabalBuck2TestModule"
        , "import System.Exit (ExitCode (..), exitWith)"
        , ""
        , "main :: IO ()"
        , "main = do"
        , "  ts <- CabalBuck2TestModule.tests"
        , "  oks <- mapM runTest ts"
        , "  exitWith (if and oks then ExitSuccess else ExitFailure 1)"
        , ""
        , "runTest :: Test -> IO Bool"
        , "runTest (Test ti) = run ti >>= report (name ti)"
        , "runTest (Group _ _ ts) = and <$> mapM runTest ts"
        , "runTest (ExtraOptions _ t) = runTest t"
        , ""
        , "report :: String -> Progress -> IO Bool"
        , "report n (Progress msg next) = putStrLn (n ++ \": \" ++ msg) >> next >>= report n"
        , "report n (Finished Pass) = putStrLn (n ++ \": PASS\") >> return True"
        , "report n (Finished (Fail msg)) = putStrLn (n ++ \": FAIL: \" ++ msg) >> return False"
        , "report n (Finished (Error msg)) = putStrLn (n ++ \": ERROR: \" ++ msg) >> return False"
        ]

-- | The @srcs@ dict key a main-is module should be relocated to (see
-- @buck2\/haskell.bzl@'s @_resolve_src@: it always relocates a main-is
-- not already named exactly this key, via a plain @export_file()@
-- copy) - the real source's own extension, not unconditionally
-- @Main.hs@. Matters because GHC only invokes its literate preprocessor
-- (@-pgmL@\/@unlit@) based on a source file's *extension*, not any
-- flag: relocating a literate @.lhs@ main-is to a plain @Main.hs@ name
-- would silently disable literate parsing even with
-- @-pgmL markdown-unlit@ still passed, and GHC would choke on the raw
-- Markdown as if it were Haskell source.
--
-- A @{-\# OPTIONS_GHC ... -F ... -pgmF \<tool\> ... \#-}@ pragma
-- embedded in the main-is file's own content (e.g. @hspec-discover@'s
-- own auto-discovery convention) is a known, undetected limitation:
-- unlike a real @ghc-options:@ flag (e.g. @-pgmL markdown-unlit@, which
-- reaches GHC completely unmodified and resolves via @PATH@ - see
-- 'compilerFlagsArg'\/@buck2\/toolchains\/BUCK@'s own @compile_env@),
-- @hspec-discover@ specifically discovers sibling @*Spec.hs@ modules by
-- scanning its own argument's *directory*, and buck2's own per-file
-- relocation (needed regardless of this, to give the module its own
-- correct path) leaves it scanning a synthetic, single-file directory -
-- silently finding zero specs rather than failing outright. Not
-- detected here (deliberately: an earlier revision scanned main-is
-- content for the pragma to skip the component with a warning instead -
-- reverted as its own kind of layering violation, reading and
-- pattern-matching GHC-internal pragma syntax from inside a Cabal-level
-- tool).
mainSrcKeyFor :: String -> String
mainSrcKeyFor mainIsRel = "Main" ++ takeExtension mainIsRel

-- | 'Nothing' if the main-is file couldn't be found - see 'resolveModules'
-- for why the caller must skip the whole component rather than emit a
-- rule pointing at a nonexistent file.
resolveMainIs :: Verbosity -> FilePath -> BuildInfo -> FilePath -> IO (Maybe String)
resolveMainIs verbosity pkgDir bi mainIs = do
  found <- firstExisting pkgDir (sourceDirs bi) [mainIs]
  case found of
    Just real -> return (Just real)
    Nothing -> do
      warn verbosity $
        "cabal buck2: couldn't find main-is file " ++ mainIs ++ " under " ++ intercalate ", " (sourceDirs bi)
      return Nothing

firstExisting :: FilePath -> [FilePath] -> [FilePath] -> IO (Maybe FilePath)
firstExisting pkgDir dirs candidates = fmap (uncurry (</>)) <$> firstExistingIn pkgDir dirs candidates

-- | Like 'firstExisting', but keeps the source directory and the file
-- relative to it apart.
firstExistingIn :: FilePath -> [FilePath] -> [FilePath] -> IO (Maybe (FilePath, FilePath))
firstExistingIn pkgDir dirs candidates =
  listToMaybe . catMaybes
    <$> sequenceA
      [ do
        exists <- doesFileExist (pkgDir </> dir </> candidate)
        return (if exists then Just (dir, candidate) else Nothing)
      | dir <- dirs
      , candidate <- candidates
      ]

-- Note [Sources outside the package directory]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A @hs-source-dirs@ entry can point outside the package directory, for
-- example @../ghc-boot-th@ in GHC's @ghc-boot-th-next@ package. buck2
-- rejects a @..@ path in @srcs@: a file belongs to the buck2 package of
-- its own directory and only that package can reference it by path.
--
-- So we reference such a source by a target label instead:
--
--   'GHC/Lexeme.hs': '//libraries/ghc-boot-th:GHC/Lexeme.hs'
--
-- The buck2 package of the label is the nearest directory, from the
-- source directory up to the project root, that has a @BUCK@ file; the
-- target name is the file path relative to that directory. For
-- @../ghc-internal/src@ this gives @//libraries/ghc-internal:src/GHC/...@
-- when @libraries/ghc-internal/BUCK@ exists, so that the ghc-internal
-- package itself can still use its @src/@ files by path (a @BUCK@ file in
-- @src/@ would make them members of another package). Without any @BUCK@
-- file, the source directory itself is used.
--
-- A hand-maintained @BUCK@ file in that directory must export the files
-- under these names, for example:
--
--   [export_file(name = f, src = f, visibility = ["PUBLIC"]) for f in glob(["src/**/*.hs"])]
--
-- A source directory outside the project root cannot be handled this way;
-- 'outsideSourceLabel' returns 'Nothing' and the path is used as-is.

-- | The target label for a source in a directory outside the package
-- directory, or 'Nothing' when the directory is inside the package.
-- The first argument is the project root.
-- See Note [Sources outside the package directory].
outsideSourceLabel :: FilePath -> FilePath -> FilePath -> IO (FilePath -> Maybe String)
outsideSourceLabel projectRoot rootRelPkgDir dir
  | ".." `notElem` splitDirectories dir = return (const Nothing)
  | otherwise = case collapse (splitDirectories (rootRelPkgDir </> dir)) of
      Nothing -> return (const Nothing)
      Just parts -> do
        -- The nearest ancestor (or the directory itself) with a BUCK file.
        let candidates = [splitAt n parts | n <- [length parts, length parts - 1 .. 1]]
        found <- filterM (\(pkg, _) -> doesFileExist (projectRoot </> joinPath pkg </> "BUCK")) candidates
        let (pkg, rest) = case found of
              (c : _) -> c
              [] -> (parts, [])
        return $ \file -> Just ("//" ++ intercalate "/" pkg ++ ":" ++ joinPath (rest ++ [file]))
  where
    -- Resolve "." and ".." components; Nothing when ".." escapes the
    -- project root.
    collapse = foldl' step (Just [])
    step acc "." = acc
    step (Just []) ".." = Nothing
    step (Just acc) ".." = Just (take (length acc - 1) acc)
    step (Just acc) d = Just (acc ++ [d])
    step Nothing _ = Nothing

-- | The project root, from a package directory and the same directory
-- relative to the root (@.@ for a package at the root).
pkgDirToRoot :: FilePath -> FilePath -> FilePath
pkgDirToRoot rootRelPkgDir pkgDir = iterate takeDirectory pkgDir !! depth
  where
    depth = length (filter (/= ".") (splitDirectories rootRelPkgDir))

sourceDirs :: BuildInfo -> [FilePath]
sourceDirs bi = case map getSymbolicPath (hsSourceDirs bi) of
  [] -> ["."]
  ds -> ds

-- | The include directories of a library, for its registration and its
-- dependents: the configure-generated headers (see Note [Configure build
-- type]) and the @include-dirs@ of the component. The directory @.@
-- cannot be a buck2 source (it would cover the package's sub-packages),
-- so it is an exported -I flag instead.
includeDirsArgs :: FilePath -> Maybe String -> BuildInfo -> [(String, Value)]
includeDirsArgs rootRelPkgDir confLabel bi =
  optionalListArg "include_dirs" (maybeToList confLabel ++ filter (/= ".") dirs)
    ++ optionalListArg "exported_preprocessor_flags" dotFlags
  where
    dirs = map (normalise . getSymbolicPath) (includeDirs bi)
    dotFlags = ["-I" ++ (if rootRelPkgDir == "." then "." else rootRelPkgDir) | "." `elem` dirs]

-- Note [Configure build type]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A package of build-type Configure has a configure script that writes
-- headers into the build directory (HsUnixConfig.h of unix,
-- DerivedConstants.h of GHC's rts); Cabal adds @<builddir>/<dir>@ to the
-- include path for every @include-dirs@ entry @dir@. The script runs when
-- `cabal buck2` configures the package, so the headers exist when the
-- targets are generated. 'configureIncludes' copies them to
-- @cabal-buck2/autogen/<target>-configure-include/@, exported as a
-- filegroup of that name, and the directory becomes an include directory
-- of the library: -I for its own compilation (GHC, hsc2hs, the C sources)
-- and @include_dirs@ for the registration and the dependents.
--
-- The hooked build info the script writes (<pkg>.buildinfo) is read by
-- 'hookedBuildInfo'.

-- | The headers a configure script generated into the build directory,
-- copied into the autogen directory: the filegroup name and its entries,
-- or 'Nothing' when there are none. See Note [Configure build type].
configureIncludes :: Variant -> FilePath -> String -> LocalBuildInfo -> BuildInfo -> IO (Maybe (String, [(String, FilePath)]))
configureIncludes variant pkgDir targetName lbi bi = do
  headers <- fmap concat $ for (ordNub (map getSymbolicPath (includeDirs bi))) $ \d -> do
    let dir = build_dir </> d
    exists <- doesDirectoryExist dir
    if exists then map (\f -> (f, dir </> f)) <$> header_files dir "" else return []
  if null headers
    then return Nothing
    else do
      for_ headers $ \(rel, src) -> do
        createDirectoryIfMissing True (takeDirectory (out_dir </> rel))
        copyFile src (out_dir </> rel)
      return (Just (name, [(rel, name </> rel) | (rel, _) <- headers]))
  where
    build_dir = interpretSymbolicPath (mbWorkDirLBI lbi) (buildDir lbi)
    name = targetName ++ "-configure-include"
    out_dir = pkgDir </> variantAutogenDir variant </> name
    -- The .h files under dir, relative to dir; Cabal's own autogen
    -- directory (cabal_macros.h, Paths_<pkg>.hs) is left out.
    header_files dir rel = do
      entries <- listDirectory (dir </> rel)
      fmap concat $ for entries $ \e -> do
        let rel_e = if null rel then e else rel </> e
        is_dir <- doesDirectoryExist (dir </> rel_e)
        if is_dir
          then if e == "autogen" then return [] else header_files dir rel_e
          else return [rel_e | takeExtension e == ".h"]

-- Note [C and Cmm sources]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
-- The @c-sources@, @cxx-sources@, @asm-sources@ and @cmm-sources@ (C--,
-- GHC's own low-level language) of a component are compiled by GHC: they
-- go into the @haskell_library@'s @srcs@ next to the modules
-- (buck2/haskell.bzl passes them on to GHC, which compiles them in its
-- --make run), with @cc-options@ as @-optc@ flags, @cxx-options@ as
-- @-optcxx@ flags and @include-dirs@ as @-I@ flags. The objects land in
-- the library's archive, as with Cabal. This matters for a program that
-- the resulting compiler links by itself (GHC's stage 2): the C objects
-- and the Haskell objects of a library reference each other
-- (ghc-internal's RtsIface.c uses closures of its modules, the modules
-- import C functions), which the linker resolves within one archive, and
-- GHC links one archive per unit of the package db.
--
-- The header files of the component (its @include-dirs@, the
-- directories of its C sources) are listed in @srcs@ too: they are not
-- compiler inputs, but buck2 reruns the compilation when they change.
--
-- A component with @pkgconfig-depends@ keeps a separate @cxx_library@ for
-- its C sources ('cxxLibraryFor'): buck2 resolves the pkg-config flags
-- there.
--
-- The C sources of an executable are compiled by GHC too and linked as
-- objects, before the libraries, which a C @main@ (GHC's iserv,
-- @-no-hs-main@) needs: in an archive after the RTS, its reference to
-- @hs_main@ would stay unresolved.

-- | The header files of a component, relative to the package directory:
-- the @.h@ files under its @include-dirs@ (not under @.@: only the
-- top-level ones there) and next to its C sources. See Note [C and Cmm
-- sources].
headerSources :: FilePath -> BuildInfo -> IO [FilePath]
headerSources pkgDir bi = do
  let include_dirs = map (normalise . getSymbolicPath) (includeDirs bi)
      c_dirs = ordNub (map (takeDirectory . normalise . getSymbolicPath) (cSources bi ++ cxxSources bi))
  under_includes <- fmap concat $ for include_dirs $ \d ->
    if d == "." then top_level "." else map (d </>) <$> recursive d
  next_to_sources <- fmap concat $ for (filter (`notElem` include_dirs) c_dirs) top_level
  return (ordNub (under_includes ++ next_to_sources))
  where
    is_header f = takeExtension f == ".h"
    entries d = do
      exists <- doesDirectoryExist (pkgDir </> d)
      if exists then listDirectory (pkgDir </> d) else return []
    top_level d = do
      es <- entries d
      return [normalise (d </> e) | e <- es, is_header e]
    -- paths relative to d
    recursive d = do
      es <- entries d
      fmap concat $ for es $ \e -> do
        is_dir <- doesDirectoryExist (pkgDir </> d </> e)
        if is_dir
          then map (e </>) <$> recursive (d </> e)
          else return [e | is_header e]

-- | Sources of a @c-sources@\/@cmm-sources@ field with the per-file options
-- of the Stable Haskell Cabal syntax: @Jumps_V32.cmm (-mavx2)@. Upstream
-- Cabal reads that as two entries, the second one @(-mavx2)@; here it is
-- paired with the file again. The options become the file's
-- @per_src_flags@ in buck2/haskell.bzl.
sourcesWithOptions :: [FilePath] -> [(FilePath, [String])]
sourcesWithOptions = go
  where
    go (f : opts : rest)
      | Just inner <- parenthesised opts = (f, words inner) : go rest
    go (f : rest) = (f, []) : go rest
    go [] = []
    parenthesised t
      | "(" `isPrefixOf` t, ")" `isSuffixOf` t = Just (take (length t - 2) (drop 1 t))
      | otherwise = Nothing

-- Note [Packages without Haskell modules]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Some Cabal packages have no Haskell module at all. GHC's tree has:
--
--   * header-only libraries: @include-dirs@ and @install-includes@ only
--     (rts-headers);
--   * C-only libraries: @c-sources@ and @install-includes@ (rts-fs), or
--     Cmm sources (the rts ways);
--   * C executables: @main-is: unlit.c@ (unlit).
--
-- A library is a registered unit even without modules: GHC reads a
-- unit's include-dirs (DerivedConstants.h of the rts unit), and a
-- program built with the resulting compiler is linked against the unit's
-- library (rts-fs) through its registration. So a header-only library is
-- a @haskell_library()@ with no sources, and a library with C or Cmm
-- sources has them compiled by GHC (see Note [C and Cmm sources]). A C
-- executable, which GHC cannot link ("no input files" with only
-- @Main.c@), becomes a @cxx_binary@ with @main-is@ and the C sources.
--
-- Dependents keep the same labels, and get the headers of these units
-- as -I flags (@include_dirs@) for the Haskell preprocessor and the C
-- compiler.

-- | @-I@ flags for a component's @include-dirs@, relative to the project
-- root: every cxx action runs with the project root as its cwd, so a
-- package-relative @include-dirs@ entry (e.g. @cbits@) needs the package
-- directory folded in by hand (confirmed empirically: a bare @-Icbits@
-- for a non-root package fails with "file not found").
includeFlagsFor :: FilePath -> BuildInfo -> [String]
includeFlagsFor rootRelPkgDir bi =
  ["-I" ++ (if rootRelPkgDir == "." then d else rootRelPkgDir </> d) | dir <- includeDirs bi, let d = getSymbolicPath dir]

-- | A 'cxx_library' named @cxxTargetName@ for a component's
-- @cxx-sources@\/@c-sources@, plus an @external_pkgconfig_library@ for each
-- distinct @pkgconfig-depends@ it needs - or nothing at all if the
-- component has no C\/C++ sources.
cxxLibraryFor
  :: Variant
  -> LocalPackageIndex
  -> FilePath
  -> FilePath
  -> String
  -> [String]
  -- ^ deps of the component (labels): their C headers are needed too
  -> [String]
  -- ^ extra exported preprocessor flags (the configure-generated headers)
  -> BuildInfo
  -> IO ([(String, [String])], [String], [Call])
cxxLibraryFor variant _localIndex rootRelPkgDir _pkgDir cxxTargetName componentDeps extraPPFlags bi
  | null srcs = return ([], [], [])
  | otherwise =
      return
        ( ("//buck2:cxx.bzl", ["cxx_library"])
            : [("@prelude//third-party:pkgconfig.bzl", ["external_pkgconfig_library"]) | not (null pkgconfigNames)]
        , [":" ++ cxxTargetName]
        , pkgconfigCalls ++ [cxxCall]
        )
  where
    srcs = map getSymbolicPath (cSources bi ++ cxxSources bi ++ asmSources bi)
    -- @exported_preprocessor_flags@ is a plain string list, so the include
    -- paths need the package directory folded in - see 'includeFlagsFor'.
    includeFlags = includeFlagsFor rootRelPkgDir bi ++ extraPPFlags
    pkgconfigNames = ordNub [unPkgconfigName n | PkgconfigDependency n _ <- pkgconfigDepends bi]
    pkgconfigCalls =
      [ call
        "external_pkgconfig_library"
        [("name", str ("pkgconfig-" ++ n)), ("package", str n), ("visibility", strList ["PUBLIC"])]
      | n <- pkgconfigNames
      ]
    -- buck2/cxx.bzl's cxx_library() wrapper adds -std=c++20 to
    -- compiler_flags whenever cxx_std isn't explicitly turned off - and
    -- that flag applies to every source in the target, C included, so a
    -- component with c-sources but no cxx-sources needs it turned off
    -- entirely (clang/gcc reject -std=c++20 for a plain .c compile).
    cxxCall =
      call
        "cxx_library"
        ( [ ("name", str cxxTargetName)
          , ("srcs", strList srcs)
          ]
            ++ optionalListArg "exported_preprocessor_flags" includeFlags
            -- buck2's compiler_flags apply to C and C++ sources alike, so
            -- @cc-options@ and @cxx-options@ are both passed.
            ++ optionalListArg "compiler_flags" (ccOptions bi ++ cxxOptions bi)
            ++ optionalListArg "deps" ([":pkgconfig-" ++ n | n <- pkgconfigNames] ++ componentDeps)
            ++ [("default_target_platform", str plat) | Just plat <- [variantPlatform variant]]
            ++ [("visibility", strList ["PUBLIC"])]
            ++ [("cxx_std", VBool False) | null (cxxSources bi)]
        )
