-- | Writes the per-package @BUCK.cabal.bzl@\/@BUCK@\/@cabal-buck2\/autogen\/
-- BUCK@ files for every local project package.
--
-- Three files, not one, per package directory:
--
--   * @BUCK.cabal.bzl@ is fully regenerated on every run (it's marked
--     @\@generated@ and never hand-edited) and defines a single
--     @generated_targets()@ macro with one rule call per buildable
--     component. Deliberately kept at the package root (*not* under
--     @cabal-buck2\/@ alongside the generated @Paths_\<pkg\>@ module and
--     each component's own @cabal_macros.h@ - see
--     "Distribution.Client.Buck2.CabalToBuck") - buck2's @load()@ only
--     accepts a bare same-package filename for a same-package path (no
--     @\/@ allowed - see 'renderBuckWrapper's own haddock), and a fully
--     cell-qualified path instead would make every hand-maintained
--     @BUCK@ depend on its own package's location in the project, which
--     defeats the point of it being freely hand-editable\/relocatable.
--   * @BUCK@ is created only if it doesn't already exist, as a two-line
--     file that loads and calls that macro. This is the file a user is
--     free to hand-edit - to add extra targets, or stop calling
--     @generated_targets()@ altogether for a package that needs fully
--     custom rules - without a re-run of @cabal buck2@ ever touching it.
--   * @cabal-buck2\/autogen\/BUCK@ (see 'generateAutogenBuck') is fully
--     regenerated on every run too, and gives every autogen file its own
--     real, addressable buck2 target (via @export_file()@) - both so a
--     generated rule's own @cabal_component@ kwarg picks up
--     @cabal_macros.h@ as a real, buck2-tracked dependency edge instead
--     of an untracked path string folded into @compiler_flags@, and so
--     that other, hand-written @BUCK@ files anywhere in the project (not
--     just this package's own) can reference e.g. @Paths_\<pkg\>@
--     directly, instead of hand-rolling a stand-in for it.
module Distribution.Client.Buck2.Generate
  ( generateAllPackages
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (makeRelative, normalise, takeDirectory, takeFileName, (</>))
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Distribution.ModuleName as ModuleName
import Distribution.Package (packageName, packageVersion)
import Distribution.Version (Version)
import Distribution.PackageDescription
  ( Library (exposedModules, reexportedModules)
  , PackageDescription
  , library
  )
import Distribution.Types.ComponentName (ComponentName)
import Distribution.Types.LocalBuildInfo (LocalBuildInfo, flagAssignment)
import Distribution.Types.Flag (unFlagAssignment, unFlagName)
import Distribution.Types.ModuleReexport
  ( ModuleReexport (moduleReexportOriginalName, moduleReexportOriginalPackage)
  )
import Distribution.Types.PackageName (PackageName, unPackageName)

import Distribution.Simple.Utils (notice, ordNub, warn)

import Distribution.Client.Buck2.CabalToBuck
import Distribution.Client.Buck2.Variant
import Distribution.Client.Buck2.Starlark

-- | Generate\/refresh @BUCK.cabal.bzl@ (and @BUCK@, where missing) for
-- every local package. @projectRoot@ is the buck2 cell root (the
-- directory containing @.buckconfig@), used to turn each package's
-- absolute directory into the cell-relative one buck2 target labels need.
-- @componentLBIs@ is a real, Cabal-computed 'LocalBuildInfo' for every
-- local (or quasi-local) *component* - see "Distribution.Client.CmdBuck2"
-- - used to generate each component's own @cabal_macros.h@\/
-- @Paths_\<pkg\>@\/@PackageInfo_\<pkg\>@ via Cabal's own real generators
-- (see "Distribution.Client.Buck2.CabalToBuck") instead of reimplementing
-- pieces of them by hand. @externalBuildTools@ is every
-- @build-tool-depends:@ executable name "Distribution.Client.Buck2.
-- Prebuilt" resolved a real external binary (and generated an
-- @export_file()@ target) for - see 'CabalToBuck.buildToolDependsArg'.
generateAllPackages :: Verbosity -> Variant -> FilePath -> FilePath -> Map (PackageName, ComponentName) LocalBuildInfo -> Set String -> [(FilePath, PackageDescription)] -> IO ()
generateAllPackages verbosity variant projectRoot unpackedRoot componentLBIs externalBuildTools pkgs = do
  names <- traverse (generateOnePackage verbosity variant localIndex projectRoot componentLBIs externalBuildTools) pkgs
  generateUnpackedAliases verbosity variant projectRoot unpackedRoot (concat names)
  -- See Note [Variant stubs]
  when (isNothing (variantName variant)) $ writeVariantStubs verbosity projectRoot
  where
    localIndex :: LocalPackageIndex
    localIndex =
      Map.fromList
        [ (packageName pkgDesc, (rootRelativeDir projectRoot pkgDir, reexportOrigins pkgDesc))
        | (pkgDir, pkgDesc) <- pkgs
        ]
    -- Every module exposed by any local package's main library, to
    -- resolve a `reexported-modules:` entry that (as is typical - see
    -- Cabal.cabal's own reexport of Cabal-syntax) names only the bare
    -- module, not an explicit `origin-package:Module` - Cabal itself
    -- resolves that form by searching the reexporting package's own
    -- build-depends for whichever one actually defines it, which for a
    -- *local* origin this index can do too (an external origin doesn't
    -- need this: its real .conf file already declares the reexport
    -- directly to ghc-pkg).
    moduleOwners :: Map.Map ModuleName.ModuleName PackageName
    moduleOwners =
      Map.fromList
        [ (m, packageName pkgDesc)
        | (_, pkgDesc) <- pkgs
        , Just lib <- [library pkgDesc]
        , m <- exposedModules lib
        ]
    reexportOrigins pkgDesc =
      ordNub
        [ pn
        | Just lib <- [library pkgDesc]
        , reexport <- reexportedModules lib
        , Just pn <- [originPackage reexport]
        , pn /= packageName pkgDesc
        ]
    originPackage reexport = case moduleReexportOriginalPackage reexport of
      Just pn -> Just pn
      Nothing -> Map.lookup (moduleReexportOriginalName reexport) moduleOwners

rootRelativeDir :: FilePath -> FilePath -> FilePath
rootRelativeDir projectRoot pkgDir = case makeRelative projectRoot pkgDir of
  "" -> "."
  rel -> rel

-- | Generate the files of one package; returns its targets (package
-- directory, package name, target name).
generateOnePackage :: Verbosity -> Variant -> LocalPackageIndex -> FilePath -> Map (PackageName, ComponentName) LocalBuildInfo -> Set String -> (FilePath, PackageDescription) -> IO [(FilePath, PackageName, String)]
generateOnePackage verbosity variant localIndex projectRoot componentLBIs externalBuildTools (pkgDir, pkgDesc) = do
  targets <- generatePackageTargets verbosity variant localIndex (rootRelativeDir projectRoot pkgDir) componentLBIs externalBuildTools pkgDir pkgDesc
  let pkgName = packageName pkgDesc
  if null (ptCalls targets)
    then do
      warn verbosity $ "cabal buck2: no buck2 targets generated for package " ++ show pkgName
      return []
    else do
      created <- writeGeneratedFiles variant pkgDir (renderGeneratedBzl pkgName (packageVersion pkgDesc) (packageFlags pkgName componentLBIs) targets)
      generateAutogenBuck variant pkgDir pkgName targets
      notice verbosity $
        "cabal buck2: generated "
          ++ (rootRelativeDir projectRoot pkgDir </> variantBzlFile variant)
          ++ " ("
          ++ show (length (ptCalls targets))
          ++ " target(s))"
          ++ (if created then ", created " ++ (rootRelativeDir projectRoot pkgDir </> "BUCK") else "")
      return [(pkgDir, pkgName, name) | Call _ args <- ptCalls targets, Just (VStr name) <- [lookup "name" args]]

-- | Write a directory's @BUCK.cabal.bzl@ (the variant's file) and its
-- @BUCK@ wrapper when missing; 'True' when the wrapper was created.
writeGeneratedFiles :: Variant -> FilePath -> String -> IO Bool
writeGeneratedFiles variant dir bzl = do
  let bzlPath = dir </> variantBzlFile variant
      buckPath = dir </> "BUCK"
  writeFile bzlPath bzl
  buckExists <- doesFileExist buckPath
  if buckExists
    then do
      -- A variant's call is appended to an existing (hand-maintained)
      -- BUCK that does not load the variant's file yet, directly or
      -- through a .bzl file of the directory; the base variant never
      -- touches an existing BUCK. See Note [Variants].
      contents <- readFile buckPath
      length contents `seq` return ()
      loaded <- for (localBzlFiles contents) $ \f -> do
        exists <- doesFileExist (dir </> f)
        if exists then readFile (dir </> f) else return ""
      let mentioned = any (variantBzlFile variant `isInfixOf`) (contents : loaded)
      when (isJust (variantName variant) && not mentioned) $
        appendFile buckPath (variantWrapperAppendix variant)
    else writeFile buckPath (renderBuckWrapper variant)
  return (not buckExists)
  where
    -- The `.bzl` files of the same directory a BUCK file loads.
    localBzlFiles s =
      [ takeWhile (/= '"') chunk
      | chunk <- drop 1 (splitOn "load(\":" s)
      ]

-- Note [Aliases for unpacked packages]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A tarball or source-repository package of the project is unpacked by
-- cabal under @dist-newstyle/src/<name>-<version or hash>/@, so its
-- labels change with its version or commit. The directory
-- @dist-newstyle/src@ gets a generated @BUCK.cabal.bzl@ with one
-- @alias()@ per target of these packages, under the target's own name:
--
--   //dist-newstyle/src:bytestring-stage2
--
-- is the label a hand-written rule (GHC's installation, buck2-ghc/BUCK)
-- uses. A target name that two packages share (the library @hpc@ and the
-- executable @hpc@ of hpc-bin) is qualified: @hpc/hpc-stage2@,
-- @hpc-bin/hpc-stage2@.

-- | The alias file of the unpacked packages' directory. See Note
-- [Aliases for unpacked packages].
generateUnpackedAliases :: Verbosity -> Variant -> FilePath -> FilePath -> [(FilePath, PackageName, String)] -> IO ()
generateUnpackedAliases verbosity variant projectRoot unpackedRoot targets
  | null aliases = return ()
  | otherwise = do
      _ <- writeGeneratedFiles variant unpackedRoot (renderGeneratedAliases variant aliases)
      notice verbosity $
        "cabal buck2: generated "
          ++ (rootRelativeDir projectRoot unpackedRoot </> variantBzlFile variant)
          ++ " ("
          ++ show (length aliases)
          ++ " alias(es))"
  where
    unpacked =
      [ (pkgDir, pkgName, name)
      | (pkgDir, pkgName, name) <- targets
      , normalise unpackedRoot `isPrefixOf` normalise pkgDir
      ]
    shared = Map.keysSet (Map.filter (> (1 :: Int)) (Map.fromListWith (+) [(name, 1) | (_, _, name) <- unpacked]))
    aliases =
      [ (if name `Set.member` shared then unPackageName pkgName ++ "/" ++ name else name, localTargetLabel (rootRelativeDir projectRoot pkgDir) name)
      | (pkgDir, pkgName, name) <- unpacked
      ]

renderGeneratedAliases :: Variant -> [(String, String)] -> String
renderGeneratedAliases variant aliases =
  unlines
    [ "# @generated by `cabal buck2` - do not edit by hand."
    , "# One alias per target of the packages cabal unpacked here, so that"
    , "# their labels do not change with the version or commit."
    , "# See Note [Aliases for unpacked packages] in Distribution.Client.Buck2.Generate."
    ]
    ++ "\n"
    ++ generatedConstant True
    ++ "\n"
    ++ "def generated_targets(overrides = {}):\n"
    ++ indentBlock (intercalate "\n" [renderCall (call "native.alias" ([("name", str name), ("actual", str actual)] ++ platform ++ [("visibility", strList ["PUBLIC"])])) | (name, actual) <- aliases])
  where
    -- An alias has the variant's platform like the target it names:
    -- `buck2 build //...` configures a top-level target without one with
    -- the default platform.
    platform = [("default_target_platform", str plat) | Just plat <- [variantPlatform variant]]

-- | @GENERATED = True@ in a generated file, @False@ in a stub. See Note
-- [Variant stubs].
generatedConstant :: Bool -> String
generatedConstant b = "GENERATED = " ++ (if b then "True" else "False") ++ "\n"

-- Note [Variant stubs]
-- ~~~~~~~~~~~~~~~~~~~~
-- A hand-maintained @BUCK@ or @.bzl@ file that loads the generated file
-- of a variant (@BUCK.stage2.cabal.bzl@) fails to parse until that
-- variant is generated, and GHC's stage 2 can only be generated once stage 1 is
-- built. A @load@ is unconditional, so the file must exist: the base
-- variant's generation writes a stub for every such file that is
-- missing, with the constants a generated file has (@UNIT_IDS@,
-- @VERSION@, @FLAGS@) and a @generated_targets()@ that defines nothing.
-- The constant @GENERATED@ is @False@ in a stub and @True@ in a generated
-- file: a hand-maintained rule of the variant tests it, so that the
-- targets of a variant exist only once it is generated (and @//...@ is
-- the base variant until then).

-- | Write a stub for every variant file a @BUCK@ file of the project
-- loads that does not exist. See Note [Variant stubs].
writeVariantStubs :: Verbosity -> FilePath -> IO ()
writeVariantStubs verbosity projectRoot = do
  buckFiles <- findBuckFiles projectRoot
  for_ buckFiles $ \buck -> do
    contents <- readFile buck
    length contents `seq` return ()
    for_ (loadedVariantFiles (takeDirectory buck) contents) $ \path -> do
      exists <- doesFileExist path
      unless exists $ do
        notice verbosity $ "cabal buck2: " ++ makeRelative projectRoot buck ++ " loads " ++ makeRelative projectRoot path ++ ", not generated yet: writing a stub"
        writeFile path stub
  where
    stub =
      unlines
        [ "# @generated stub by `cabal buck2`: the BUCK file loads this variant's"
        , "# file, but the variant has not been generated yet (see Note [Variant"
        , "# stubs] in Distribution.Client.Buck2.Generate). `cabal buck2 --variant`"
        , "# replaces it."
        , ""
        , "UNIT_IDS = {}"
        , ""
        , "VERSION = '0'"
        , ""
        , "FLAGS = {}"
        , ""
        , generatedConstant False
        , "def generated_targets(overrides = {}):"
        , "    pass"
        ]
    -- The `BUCK.<name>.cabal.bzl` files a BUCK or .bzl file in `dir`
    -- loads, as paths: `load(":X")` is in the same directory,
    -- `load("//pkg:X")` in the package's directory (other cells are left
    -- alone).
    loadedVariantFiles dir s =
      [ path
      | chunk <- drop 1 (splitOn "load(\"" s)
      , let label = takeWhile (/= '"') chunk
            (pkg, file) = case break (== ':') label of
              (p, ':' : f) -> (p, f)
              _ -> ("", "")
      , "BUCK." `isPrefixOf` file
      , ".cabal.bzl" `isSuffixOf` file
      , file /= "BUCK.cabal.bzl"
      , Just path <- [resolve pkg file]
      ]
      where
        resolve "" file = Just (dir </> file)
        resolve pkg file
          | "//" `isPrefixOf` pkg = Just (projectRoot </> drop 2 pkg </> file)
          | otherwise = Nothing

-- | Split a string on a separator.
splitOn :: String -> String -> [String]
splitOn sep = go
  where
    go t = case breakOn t of
      (before, Nothing) -> [before]
      (before, Just rest) -> before : go rest
    breakOn t
      | sep `isPrefixOf` t = ("", Just (drop (length sep) t))
      | otherwise = case t of
          [] -> ("", Nothing)
          c : cs -> let (b, r) = breakOn cs in (c : b, r)

-- | The @BUCK@ and hand-maintained @.bzl@ files under a directory,
-- leaving out buck2's output, the repositories and the generated files
-- (cabal's build directory is included: the packages it unpacks have
-- BUCK files).
findBuckFiles :: FilePath -> IO [FilePath]
findBuckFiles dir = do
  entries <- listDirectory dir
  fmap concat $ for entries $ \e -> do
    let path = dir </> e
    isDir <- doesDirectoryExist path
    if isDir
      then if e `elem` [".git", "buck-out"] then return [] else findBuckFiles path
      else return [path | e == "BUCK" || (".bzl" `isSuffixOf` e && not (".cabal.bzl" `isSuffixOf` e))]

-- | @cabal-buck2\/autogen\/BUCK@: one @export_file()@ per generated
-- autogen file (a component's own @cabal_macros.h@, or the package's
-- @Paths_\<pkg\>@ module - see 'PackageTargets'' own haddock), so it's a
-- real, addressable buck2 target - referenced by this same package's own
-- generated rules via their @cabal_component@ kwarg (see
-- "Distribution.Client.Buck2.CabalToBuck"'s @cabalComponentArg@), and
-- just as easily by a hand-written rule anywhere else in the project
-- (the point of this file existing at all - see buck2.md's DONE entry on
-- this). Fully regenerated on every run, like @BUCK.cabal.bzl@ itself -
-- never hand-edited, so no separate wrapper file is needed here the way
-- @BUCK@ is for it. @export_file@ is a builtin buck2\/prelude rule, so
-- this needs no @load()@ statement at all.
generateAutogenBuck :: Variant -> FilePath -> PackageName -> PackageTargets -> IO ()
generateAutogenBuck variant pkgDir pkgName targets
  | null exports = return ()
  | otherwise = do
      createDirectoryIfMissing True autogenDir
      writeFile (autogenDir </> "BUCK") (renderFile header [] exportCalls)
  where
    exports = ptAutogenExports targets
    autogenDir = pkgDir </> variantAutogenDir variant
    header =
      "@generated by `cabal buck2` from "
        ++ prettyShow pkgName
        ++ ".cabal - do not edit by hand.\nRe-run `cabal buck2` after editing the .cabal file to refresh this file."
    exportCalls = map exportCall exports
    exportCall (AutogenFile exportName exportRelPath) =
      call
        "export_file"
        [ ("name", str exportName)
        , ("src", str exportRelPath)
        , -- export_file()'s own `out` defaults to the *rule's* name, not
          -- to `src`'s basename (see buck2/prelude/export_file.bzl) - so
          -- without this, the materialised artifact would be named e.g.
          -- `exe-pkg-detailed-test-stub-main` instead of `Main.hs`,
          -- losing the source file's real extension. Harmless for a
          -- consumer that only ever references the file opaquely (e.g.
          -- cabal_component's own `$(location ...)` include-path use),
          -- but a real, silent bug for one referenced from `srcs`:
          -- buck2/haskell.bzl's own `is_haskell_src()` check (and GHC's
          -- own module-name-from-extension logic) both key off the
          -- artifact's own filename, not the target label - an artifact
          -- missing its `.hs` extension is silently treated as a
          -- non-Haskell "hidden" input instead of a compiled module,
          -- with no error from buck2 itself, just GHC's own unhelpful
          -- "Could not find module" (found the hard way against a real
          -- `buck2 build` - see buck2.md's own entry on this).
          ("out", str (takeFileName exportRelPath))
        , ("visibility", strList ["PUBLIC"])
        ]
    -- A directory of headers (see Note [Configure build type] in
    -- Distribution.Client.Buck2.CabalToBuck): the filegroup's output
    -- directory has the files at the dict keys.
    exportCall (AutogenDir exportName entries) =
      call
        "filegroup"
        [ ("name", str exportName)
        , ("srcs", VDict [(k, str v) | (k, v) <- entries])
        , ("visibility", strList ["PUBLIC"])
        ]

-- | The package's resolved Cabal flags, from any configured component of
-- it (they are the same for all components of a package).
packageFlags :: PackageName -> Map (PackageName, ComponentName) LocalBuildInfo -> [(String, Bool)]
packageFlags pkgName componentLBIs =
  case [lbi | ((pn, _), lbi) <- Map.toList componentLBIs, pn == pkgName] of
    [] -> []
    lbi : _ -> [(unFlagName fn, b) | (fn, b) <- unFlagAssignment (flagAssignment lbi)]

renderGeneratedBzl :: PackageName -> Version -> [(String, Bool)] -> PackageTargets -> String
renderGeneratedBzl pkgName version flags targets =
  unlines
    [ "# @generated by `cabal buck2` from " ++ prettyShow pkgName ++ ".cabal - do not edit by hand."
    , "# Re-run `cabal buck2` after editing the .cabal file to refresh this file."
    ]
    ++ "\n"
    ++ renderLoad "//buck2:cabal_overrides.bzl" ["apply_overrides"]
    ++ concatMap (uncurry renderLoad) (ptLoads targets)
    ++ "\n"
    ++ renderUnitIds targets
    ++ "\n"
    ++ "VERSION = " ++ renderValue 0 (VStr (prettyShow version)) ++ "\n"
    ++ "\n"
    ++ renderFlags flags
    ++ "\n"
    ++ generatedConstant True
    ++ "\n"
    ++ "def generated_targets(overrides = {}):\n"
    ++ indentBlock (intercalate "\n" (map renderOverridableCall (ptCalls targets)))

-- Note [Overriding generated targets]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A hand-maintained @BUCK@ file sometimes must extend a generated rule:
-- add compiler_flags (e.g. an include dir with generated files), add
-- srcs, add deps. A copy of the whole generated call in @BUCK@ would not
-- follow the .cabal file any more. So @generated_targets()@ takes an
-- optional dict, keyed by target name, of keyword arguments to merge into
-- that target's call:
--
--   generated_targets(overrides = {
--       "ghc": {"compiler_flags": ["-I$(location :primop-incls)"]},
--   })
--
-- Lists are appended, dicts are merged (the override wins on a common
-- key), any other value is replaced. The merge is done by
-- @apply_overrides@ in @buck2/cabal_overrides.bzl@ (haskell-buck2), which
-- every generated file loads.

-- | @UNIT_IDS = {target: unit id}@ of the file's libraries, for a
-- hand-maintained BUCK that needs a unit id (e.g. GHC's compiler/BUCK
-- generates GHC.Settings.Config with the unit ids of ghc and
-- ghc-internal): @load(":BUCK.cabal.bzl", "UNIT_IDS")@.
renderUnitIds :: PackageTargets -> String
renderUnitIds targets =
  renderValue 0 (VDict [(name, VStr uid) | Call _ args <- ptCalls targets, Just (VStr name) <- [lookup "name" args], Just (VStr uid) <- [lookup "unit_id" args]])
    & ("UNIT_IDS = " ++)
    & (++ "\n")
  where
    x & f = f x

-- | @FLAGS = {flag: bool}@: the package's resolved Cabal flags, for a
-- hand-maintained BUCK that runs the package's configure script (a
-- @build-type: Configure@ package reads them as @CABAL_FLAG_<flag>@, the
-- way Cabal passes them).
renderFlags :: [(String, Bool)] -> String
renderFlags flags = "FLAGS = " ++ renderValue 0 (VDict [(f, VBool b) | (f, b) <- flags]) ++ "\n"

indentBlock :: String -> String
indentBlock = unlines . map indentLine . lines
  where
    indentLine "" = ""
    indentLine l = "    " ++ l

-- | buck2's @load()@ doesn't accept a same-package path containing a
-- @\/@ (@:cabal-buck2\/targets.bzl@ fails outright: "Unable to parse
-- import spec ... but got a path") - only a bare same-package filename
-- (@:filename.bzl@) or a fully cell-qualified one
-- (@\/\/package\/path:filename.bzl@) work, and the latter would make
-- this hand-maintained file depend on its own package's location in the
-- project (confirmed empirically against the real buck2 binary while
-- trying @BUCK.cabal.bzl@ living under @cabal-buck2\/@ instead - reverted
-- for exactly this reason). Keeping @BUCK.cabal.bzl@ at the package root
-- avoids the whole issue: it's a same-package, no-slash filename either
-- way.
renderBuckWrapper :: Variant -> String
renderBuckWrapper variant =
  unlines
    [ "# Hand-maintained: add extra targets below, pass overrides = {...} to"
    , "# generated_targets() to extend a generated rule, or stop calling it to"
    , "# fully take over this package's BUCK rules."
    ]
    ++ variantWrapperBlock variant

-- | The load and call of a variant's generated targets (see Note [Variants]):
-- @generated_targets_stage2@ for the variant @stage2@, so that several
-- variants can share one BUCK file.
variantWrapperBlock :: Variant -> String
variantWrapperBlock variant = case variantName variant of
  Nothing ->
    unlines
      [ "load(\":BUCK.cabal.bzl\", \"generated_targets\")"
      , ""
      , "generated_targets()"
      ]
  Just n ->
    unlines
      [ "load(\":" ++ variantBzlFile variant ++ "\", " ++ fn n ++ " = \"generated_targets\")"
      , ""
      , fn n ++ "()"
      ]
  where
    fn n = "generated_targets_" ++ map (\c -> if c == '-' then '_' else c) n

-- | What a variant appends to an existing BUCK file.
variantWrapperAppendix :: Variant -> String
variantWrapperAppendix variant =
  "\n# Targets of variant `" ++ fromMaybe "" (variantName variant) ++ "`, added by `cabal buck2 --variant`.\n"
    ++ variantWrapperBlock variant
