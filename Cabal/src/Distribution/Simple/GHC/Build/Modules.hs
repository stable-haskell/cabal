{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Distribution.Simple.GHC.Build.Modules
  ( buildHaskellModules
  , BuildWay (..)
  , buildWayObjectExtension
  , componentInputs
  , buildWayExtraHcOptions
  ) where

import Control.Monad.IO.Class
import Distribution.Compat.Prelude

import Data.List (sortOn)
import qualified Data.Set as Set
import Distribution.CabalSpecVersion
import Distribution.ModuleName (ModuleName)
import qualified Distribution.PackageDescription as PD
import Distribution.Pretty
import Distribution.Simple.Build.Inputs
import Distribution.Simple.BuildWay
import Distribution.Simple.Compiler
import Distribution.Simple.Errors (CabalException (StandaloneBytecodeNotSupportedYet))
import Distribution.Simple.GHC.Build.Utils
import qualified Distribution.Simple.GHC.Internal as Internal
import qualified Distribution.Simple.Hpc as Hpc
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program.GHC
import Distribution.Simple.Program.Types
import Distribution.Simple.Setup.Common
import Distribution.Simple.Utils
import Distribution.Types.Benchmark
import Distribution.Types.BenchmarkInterface
import Distribution.Types.BuildInfo
import Distribution.Types.Executable
import Distribution.Types.ForeignLib
import Distribution.Types.PackageName.Magic
import Distribution.Types.ParStrat
import Distribution.Types.TestSuite
import Distribution.Types.TestSuiteInterface
import Distribution.Utils.NubList
import Distribution.Utils.Path
import Distribution.Verbosity (VerbosityHandles, mkVerbosity, verbosityLevel)
import System.FilePath ()

{-
Note [Building Haskell Modules accounting for TH]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are two artifact ways: static and dynamic/shared. Both use the selected
compiler's target ABI, including when that ABI is profiling.

Template Haskell may need the compiler's default static/dynamic way in addition
to the requested way. Build that way first so the target interpreter can load
its objects. Profiling does not add another set of artifacts or substitute the
ordinary host compiler's ABI for the target interpreter's ABI.

When both artifact ways are needed, use -dynamic-too if supported; otherwise
build the compiler's default way first. Profiling instrumentation and package
options are selected independently from the explicit target ABI.

-}

-- | Compile the Haskell modules of the component being built.
buildHaskellModules
  :: Flag ParStrat
  -- ^ The parallelism strategy (e.g. num of jobs)
  -> ConfiguredProgram
  -- ^ The GHC configured program
  -> Maybe (SymbolicPath Pkg File)
  -- ^ Optional path to a Haskell Main file to build
  -> [ModuleName]
  -- ^ The Haskell modules to build
  -> SymbolicPath Pkg ('Dir Artifacts)
  -- ^ The path to the build directory for this target, which
  -- has already been created.
  -> [BuildWay]
  -- ^ The set of needed build ways according to user options
  -> VerbosityHandles
  -- ^ Logging handles
  -> PreBuildComponentInputs
  -- ^ The context and component being built in it.
  -> IO (BuildWay -> GhcOptions)
  -- ^ Returns a mapping from build ways to the 'GhcOptions' used in the
  -- invocation used to compile the component in that 'BuildWay'.
  -- This can be useful in, eg, a linker invocation, in which we want to use the
  -- same options and list the same inputs as those used for building.
buildHaskellModules numJobs ghcProg mbMainFile inputModules buildTargetDir neededLibWays verbHandles pbci = do
  -- See Note [Building Haskell Modules accounting for TH]

  let
    verbosity = mkVerbosity verbHandles $ buildVerbosity pbci
    isLib = buildIsLib pbci
    clbi = buildCLBI pbci
    lbi = localBuildInfo pbci
    bi = buildBI pbci
    what = buildingWhat pbci
    comp = buildCompiler pbci
    i = interpretSymbolicPathLBI lbi -- See Note [Symbolic paths] in Distribution.Utils.Path
    neededLibWaysSet = Set.fromList neededLibWays

    withBytecode = withBytecodeLib lbi

    -- If this component will be loaded into a repl, we don't compile the modules at all.
    forRepl
      | BuildRepl{} <- what = True
      | otherwise = False

    -- TODO: do we need to put hs-boot files into place for mutually recursive
    -- modules?  FIX: what about exeName.hi-boot?

    -- Determine if program coverage should be enabled and if so, what
    -- '-hpcdir' should be.
    isCoverageEnabled = if isLib then libCoverage lbi else exeCoverage lbi
    hpcdir way
      | forRepl = mempty -- HPC is not supported in ghci
      | isCoverageEnabled = Flag $ Hpc.mixDir (coerceSymbolicPath $ coerceSymbolicPath buildTargetDir </> extraCompilationArtifacts) way
      | otherwise = mempty

    mbWorkDir = mbWorkDirLBI lbi
    tempFileOptions = commonSetupTempFileOptions $ buildingWhatCommonFlags what
    runGhcProg =
      runGHCWithResponseFile
        "ghc.rsp"
        Nothing
        tempFileOptions
        verbosity
        ghcProg
        comp
        platform
        mbWorkDir
    platform = hostPlatform lbi

    (hsMains, scriptMains) =
      partition (isHaskell . getSymbolicPath) (maybeToList mbMainFile)

    -- We define the base opts which are shared across different build ways in
    -- 'buildHaskellModules'
    baseOpts way =
      (Internal.componentGhcOptions (verbosityLevel verbosity) lbi bi clbi buildTargetDir)
        `mappend` mempty
          { ghcOptMode = toFlag GhcModeMake
          , -- Previously we didn't pass -no-link when building libs,
            -- but I think that could result in a bug (e.g. if a lib module is
            -- called Main and exports main). So we really want nolink when
            -- building libs too (TODO).
            ghcOptNoLink = if isLib then NoFlag else toFlag True
          , ghcOptNumJobs = numJobs
          , ghcOptInputModules = toNubListR inputModules
          , ghcOptInputFiles = toNubListR hsMains
          , ghcOptInputScripts = toNubListR scriptMains
          , ghcOptExtra = buildWayExtraHcOptions way comp bi
          , ghcOptProfilingMode = if compilerTargetsProfiling comp then toFlag True else NoFlag
          , ghcOptProfilingAuto =
              if compilerTargetsProfiling comp
                then
                  Internal.profDetailLevelFlag
                    isLib
                    ((if isLib then withProfLibDetail else withProfExeDetail) lbi)
                else NoFlag
          , ghcOptHiSuffix = needsWaySuffixFlag (buildWayInterfaceExtension way) "hi"
          , ghcOptObjSuffix = needsWaySuffixFlag (buildWayObjectExtension "o" way) "o"
          , ghcOptHPCDir = hpcdir (buildWayHpcWay way) -- maybe this should not be passed for vanilla?
          }
      where
        needsWaySuffixFlag x y = if x == y then NoFlag else toFlag x

    -- Bytecode objects are attached to an existing native way; see
    -- doc/internal/bytecode-libraries.md for the companion-way rules.
    bytecodeToo p = if p then toFlag GhcByteCodeAndObjectCode else NoFlag

    hasWay way = way `Set.member` neededLibWaysSet

    -- For libs we don't pass -static when building static, leaving it
    -- implicit. We should just always pass -static, but we don't want to
    -- change behaviour when doing the refactor.
    staticOpts =
      (baseOpts StaticWay)
        { ghcOptDynLinkMode = if isLib then NoFlag else toFlag GhcStaticOnly
        , ghcOptObjectMode = bytecodeToo (withBytecode && not (hasWay DynWay))
        }
    dynOpts =
      (baseOpts DynWay)
        { ghcOptDynLinkMode = toFlag GhcDynamicOnly -- use -dynamic
        , -- TODO: Does it hurt to set -fPIC for executables?
          ghcOptFPic = toFlag True -- use -fPIC
        , ghcOptObjectMode = bytecodeToo withBytecode
        }
    -- Options for building both static and dynamic way at the same time, using
    -- the GHC flag -static and -dynamic-too
    dynTooOpts =
      (baseOpts StaticWay)
        { ghcOptDynLinkMode = toFlag GhcStaticAndDynamic -- use -dynamic-too
        , ghcOptDynHiSuffix = toFlag (buildWayInterfaceExtension DynWay)
        , ghcOptDynObjSuffix = toFlag (buildWayObjectExtension "o" DynWay)
        , ghcOptHPCDir = hpcdir Hpc.Dyn
        , ghcOptObjectMode = bytecodeToo withBytecode
        -- Should we pass hcSharedOpts in the -dynamic-too ghc invocation?
        -- (Note that `baseOtps StaticWay = hcStaticOptions`, not hcSharedOpts)
        }

    -- Determines how to build for each way, also serves as the base options
    -- for loading modules in 'linkOrLoadComponent'
    buildOpts way = case way of
      StaticWay -> staticOpts
      DynWay -> dynOpts

  when
    ( withBytecode
        && not (hasWay StaticWay || hasWay DynWay)
    )
    $ dieWithException verbosity StandaloneBytecodeNotSupportedYet

  -- If there aren't modules, or if we're loading the modules in repl, don't build.
  unless (forRepl || (isNothing mbMainFile && null inputModules)) $ liftIO $ do
    -- See Note [Building Haskell Modules accounting for TH]
    let
      -- If we need both static and dynamic, use dynamic-too instead of
      -- compiling twice (if we support it)
      useDynamicToo =
        StaticWay `Set.member` neededLibWaysSet
          && DynWay `Set.member` neededLibWaysSet
          && supportsDynamicToo comp
          && null (buildWayExtraHcOptions DynWay comp bi)

      defaultGhcWay = compilerBuildWay comp

      order w
        | w == defaultGhcWay = 0
        | otherwise = fromEnum w + 1

      -- The ways we'll build, in order
      orderedBuilds
        -- We need to make sure that the way which is the way the compiler is built
        -- is built first so that Template Haskell works.
        | useDynamicToo = [buildStaticAndDynamicToo]
        -- Otherwise, we need to ensure the defaultGhcWay is built first
        | otherwise =
            runGhcProg . buildOpts <$> sortOn order neededLibWays

      buildStaticAndDynamicToo = do
        runGhcProg dynTooOpts
        case (hpcdir Hpc.Dyn, hpcdir Hpc.Vanilla) of
          (Flag dynDir, Flag vanillaDir) ->
            -- When the vanilla and shared library builds are done
            -- in one pass, only one set of HPC module interfaces
            -- are generated. This set should suffice for both
            -- static and dynamically linked executables. We copy
            -- the modules interfaces so they are available under
            -- both ways.
            copyDirectoryRecursive verbosity (i dynDir) (i vanillaDir)
          _ -> return ()

    sequence_ orderedBuilds
  return buildOpts

-- | Returns the corresponding 'Hpc.Way' for a 'BuildWay'
buildWayHpcWay :: BuildWay -> Hpc.Way
buildWayHpcWay = \case
  StaticWay -> Hpc.Vanilla
  DynWay -> Hpc.Dyn

-- | Returns a function to extract the extra haskell compiler options from a
-- 'BuildInfo' and the compiler's target ABI
buildWayExtraHcOptions :: BuildWay -> Compiler -> BuildInfo -> [String]
buildWayExtraHcOptions way comp
  | compilerTargetsProfiling comp = case way of
      StaticWay -> hcProfOptions GHC
      DynWay -> hcProfSharedOptions GHC
  | otherwise = case way of
      StaticWay -> hcStaticOptions GHC
      DynWay -> hcSharedOptions GHC

-- | Returns a pair of the main file and Haskell modules of the component being
-- built. The main file is not necessarily a Haskell file. It could also be
-- e.g. a C source, or, a Haskell repl script (that does not necessarily have
-- an extension).
--
-- The main file is Nothing if the component is not executable.
componentInputs
  :: SymbolicPath Pkg (Dir Artifacts)
  -- ^ Target build dir
  -> VerbosityHandles
  -- ^ Logging handles
  -> PD.PackageDescription
  -> PreBuildComponentInputs
  -- ^ The context and component being built in it.
  -> IO (Maybe (SymbolicPath Pkg File), [ModuleName])
  -- ^ The main input file, and the Haskell modules
componentInputs buildTargetDir verbHandles pkg_descr pbci =
  case component of
    CLib lib ->
      pure (Nothing, allLibModules lib clbi)
    CFLib flib ->
      pure (Nothing, foreignLibModules flib)
    CExe Executable{buildInfo = bi', modulePath} ->
      exeLikeInputs bi' modulePath
    CTest TestSuite{testBuildInfo = bi', testInterface = TestSuiteExeV10 _ mainFile} ->
      exeLikeInputs bi' mainFile
    CBench Benchmark{benchmarkBuildInfo = bi', benchmarkInterface = BenchmarkExeV10 _ mainFile} ->
      exeLikeInputs bi' mainFile
    CTest TestSuite{} -> error "testSuiteExeV10AsExe: wrong kind"
    CBench Benchmark{} -> error "benchmarkExeV10asExe: wrong kind"
  where
    verbosity = mkVerbosity verbHandles $ buildVerbosity pbci
    component = buildComponent pbci
    clbi = buildCLBI pbci
    mbWorkDir = mbWorkDirLBI $ localBuildInfo pbci
    exeLikeInputs bnfo modulePath = liftIO $ do
      main <- findExecutableMain verbosity mbWorkDir buildTargetDir (bnfo, modulePath)
      let mainModName = exeMainModuleName bnfo
          otherModNames = otherModules bnfo

      -- Scripts have fakePackageId and are always Haskell but can have any extension.
      if isHaskell (getSymbolicPath main) || PD.package pkg_descr == fakePackageId
        then
          if PD.specVersion pkg_descr < CabalSpecV2_0 && (mainModName `elem` otherModNames)
            then do
              -- The cabal manual clearly states that `other-modules` is
              -- intended for non-main modules.  However, there's at least one
              -- important package on Hackage (happy-1.19.5) which
              -- violates this. We workaround this here so that we don't
              -- invoke GHC with e.g.  'ghc --make Main src/Main.hs' which
              -- would result in GHC complaining about duplicate Main
              -- modules.
              --
              -- Finally, we only enable this workaround for
              -- specVersion < 2, as 'cabal-version:>=2.0' cabal files
              -- have no excuse anymore to keep doing it wrong... ;-)
              warn verbosity $
                "Enabling workaround for Main module '"
                  ++ prettyShow mainModName
                  ++ "' listed in 'other-modules' illegally!"
              return (Just main, filter (/= mainModName) otherModNames)
            else return (Just main, otherModNames)
        else return (Just main, otherModNames)
