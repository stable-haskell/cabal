{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Distribution.Client.InLibrary
  ( libraryConfigureInputsFromElabPackage
  , configure
  , build
  , haddock
  , copy
  , register
  , repl
  , test
  , bench
  )
where

import Distribution.Backpack.DescribeUnitId (setupMessage')
import Distribution.Client.ProjectPlanning.Types
import Distribution.Client.RebuildMonad
import qualified Distribution.Client.SetupHooks.CallHooksExe as ExternalHooksExe
  ( buildTypePreBuildHooks
  , buildTypeSetupHooks
  )
import Distribution.Client.Types

import qualified Distribution.PackageDescription as PD
import Distribution.Simple (Compiler, PackageDBStackCWD)
import qualified Distribution.Simple.Bench as Cabal
import Distribution.Simple.Build (build_setupHooks, repl_setupHooks)
import qualified Distribution.Simple.Configure as Cabal
import Distribution.Simple.Haddock (haddock_setupHooks)
import Distribution.Simple.Install (install_setupHooks)
import Distribution.Simple.LocalBuildInfo (mbWorkDirLBI)
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import qualified Distribution.Simple.PreProcess as Cabal
import Distribution.Simple.Program.Db
import qualified Distribution.Simple.Register as Cabal
import qualified Distribution.Simple.Setup as Cabal
import Distribution.Simple.SetupHooks.Internal
import qualified Distribution.Simple.Test as Cabal
import Distribution.Simple.Utils
import Distribution.System (Platform)
import Distribution.Types.BuildType
import Distribution.Types.ComponentRequestedSpec
import qualified Distribution.Types.LocalBuildConfig as LBC
import Distribution.Types.LocalBuildInfo
import Distribution.Utils.Path
  ( makeSymbolicPath
  , relativeSymbolicPath
  )
import Distribution.Verbosity
  ( VerbosityHandles
  , mkVerbosity
  )

import Distribution.Types.HookedBuildInfo (emptyHookedBuildInfo)
import Distribution.Compat.Directory (canonicalizePath)

-- Note [MHS support]
-- ~~~~~~~~~~~~~~~~~~
-- When cabal-install itself is built with MHS (MicroHS), the in-library
-- setup method is unavailable (see Note [MHS support] in
-- "Distribution.Client.SetupWrapper", which always selects the external
-- setup method). Under @__MHS__@ this module therefore only provides
-- stubs with the same types as the GHC implementation, so that callers
-- keep typechecking while the stubs are never invoked at runtime.

--------------------------------------------------------------------------------
-- Configure

data LibraryConfigureInputs = LibraryConfigureInputs
  { verbosityHandles :: VerbosityHandles
  , compiler :: Compiler
  , platform :: Platform
  , buildType :: BuildType
  , compRequested :: Maybe PD.ComponentName
  , localBuildConfig :: LBC.LocalBuildConfig
  , packageDBStack :: PackageDBStackCWD
  , packageDescription :: PD.PackageDescription
  , gPackageDescription :: PD.GenericPackageDescription
  , flagAssignment :: PD.FlagAssignment
  , installedPkgIndex :: InstalledPackageIndex
  }

libraryConfigureInputsFromElabPackage
  :: VerbosityHandles
  -> BuildType
  -> ProgramDb
  -> ElaboratedSharedConfig
  -> ElaboratedReadyPackage
  -> InstalledPackageIndex
  -> [String]
  -- ^ targets
  -> LibraryConfigureInputs
libraryConfigureInputsFromElabPackage
  verbHandles
  bt
  progDb
  -- NB: don't use the ProgramDb from the ElaboratedSharedConfig;
  -- that one is only for the compiler itself and not for the package.
  sharedConfig
  (ReadyPackage pkg)
  ipi
  userTargets =
    LibraryConfigureInputs
      { verbosityHandles = verbHandles
      , -- The package is configured with the toolchain of its own machine.
        compiler = elabCompiler sharedConfig pkg
      , platform = elabPlatform sharedConfig pkg
      , buildType =
          -- NB: don't get the build-type from 'pkgDescr',
          -- because for Configure build-type we rewrite the build-type
          -- to Simple for components that are neither the main library
          -- nor an executable.
          --
          -- See also 'isMainLibOrExeComponent'.
          bt
      , compRequested =
          case elabPkgOrComp pkg of
            ElabComponent elabComp
              | Just elabCompNm <- compComponentName elabComp ->
                  Just elabCompNm
            _ -> Nothing
      , localBuildConfig =
          LBC.LocalBuildConfig
            { LBC.extraConfigArgs = userTargets
            , LBC.withPrograms = progDb
            , LBC.withBuildOptions = elabBuildOptions pkg
            }
      , packageDBStack = elabBuildPackageDBStack pkg
      , packageDescription = pkgDescr
      , gPackageDescription = gpkgDescr
      , flagAssignment = elabFlagAssignment pkg
      , installedPkgIndex = ipi
      }
    where
      pkgDescr = elabPkgDescription pkg
      gpkgDescr = elabGPkgDescription pkg

configure
  :: LibraryConfigureInputs
  -> Cabal.ConfigFlags
  -> IO LocalBuildInfo
#ifdef __MHS__
configure _ _ =
  error "Distribution.Client.InLibrary.configure: in-library setup is not supported under MHS"
#else
configure
  LibraryConfigureInputs
    { verbosityHandles = verbHandles
    , platform = plat
    , compiler = compil
    , buildType = bt
    , compRequested = mbComp
    , localBuildConfig = lbc0
    , packageDBStack = packageDBs
    , packageDescription = pkgDescr
    , gPackageDescription = gpkgDescr
    , flagAssignment = flagAssgn
    , installedPkgIndex = ipi
    }
  cfg = do
    -- Here, we essentially want to call the Cabal library 'configure' function,
    -- but skipping over all the steps we don't need such as rediscovering the
    -- compiler or re-resolving the conditionals in the package, as we have done
    -- all of that already.
    --
    -- To achieve this, we call the Cabal 'configureFinal' function which skips
    -- these preparatory steps.
    let verbFlags = Cabal.fromFlag $ Cabal.configVerbosity cfg
        verbosity = mkVerbosity verbHandles verbFlags
        mbWorkDir = Cabal.flagToMaybe $ Cabal.configWorkingDir cfg
        distPref = Cabal.fromFlag $ Cabal.configDistPref cfg
        confHooks =
          configureHooks $
            ExternalHooksExe.buildTypeSetupHooks verbosity mbWorkDir distPref bt

    let pkgId :: PD.PackageIdentifier
        pkgId = PD.package pkgDescr

    -- cabal-install uses paths relative to the current working directory,
    -- while the Cabal library expects symbolic paths. Perform the conversion here
    -- by making the paths absolute.
    packageDBs' <- traverse (traverse $ fmap makeSymbolicPath . canonicalizePath) packageDBs

    -- Configure package
    case mbComp of
      Nothing -> setupMessage verbosity "Configuring" pkgId
      Just cname ->
        setupMessage'
          verbosity
          "Configuring"
          pkgId
          cname
          (Just (Cabal.configInstantiateWith cfg))

    -- TODO: we should implement recompilation checking on the level of
    -- individual components, so that we only re-configure the components that
    -- need reconfiguring (including running their hooks). See #11761.
    lbc1 <- case preConfPackageHook confHooks of
      Nothing -> return lbc0
      Just hk -> Cabal.runPreConfPackageHook cfg compil plat lbc0 hk
    let compRequestedSpec = case mbComp of
          Just compName -> OneComponentRequestedSpec compName
          Nothing ->
            ComponentRequestedSpec
              { testsRequested = Cabal.fromFlag (Cabal.configTests cfg)
              , benchmarksRequested = Cabal.fromFlag (Cabal.configBenchmarks cfg)
              }

    -- NB: it's OK to discard constraints here: we already have a finalized PackageDescription
    -- in hand, and we are using exact UnitIds for all dependencies (this corresponds
    -- to using --exact-configuration and --dependency flags with the Setup CLI).
    (_allConstraints, pkgInfo) <-
      -- Use cabal-install's running InstalledPackageIndex 'ipi' to skip over
      -- having to invoke ghc-pkg once per package.
      --
      -- See (ProjIPI3) from Note [Per-project InstalledPackageIndex]
      -- in Distribution.Client.ProjectBuilding.
      Cabal.computePackageInfoFromIndex verbHandles cfg gpkgDescr ipi

    -- Post-configure hooks & per-component configure
    lbi1 <-
      Cabal.configureFinal
        verbHandles
        confHooks
        emptyHookedBuildInfo
        cfg
        lbc1
        (gpkgDescr, pkgDescr)
        flagAssgn
        compRequestedSpec
        compil
        plat
        packageDBs'
        pkgInfo

    -- Remember the .cabal filename if we know it.
    pkgDescrFilePath <-
      case Cabal.flagToMaybe $ Cabal.configCabalFilePath cfg of
        Just pkgFile -> return pkgFile
        Nothing -> relativeSymbolicPath <$> tryFindPackageDesc verbosity mbWorkDir
    return $ lbi1{pkgDescrFile = Just pkgDescrFilePath}
#endif

--------------------------------------------------------------------------------
-- Build

build
  :: VerbosityHandles
  -> Cabal.BuildFlags
  -> LocalBuildInfo
  -> [String]
  -> IO [MonitorFilePath]
#ifdef __MHS__
build _ _ _ _ =
  error "Distribution.Client.InLibrary.build: in-library setup is not supported under MHS"
#else
build verbHandles flags lbi _args =
  build_setupHooks (preBuildHook, postBuildHook) verbHandles pkgDescr lbi flags Cabal.knownSuffixHandlers
  where
    verb = mkVerbosity verbHandles $ Cabal.fromFlag $ Cabal.buildVerbosity flags
    hooks = ExternalHooksExe.buildTypeSetupHooks verb mbWorkDir distPref bt
    -- (Recall that pre-build hooks are treated specially;
    -- see the 'buildTypeSetupHooks' and 'buildTypePreBuildHooks' functions.)
    preBuildHook = ExternalHooksExe.buildTypePreBuildHooks verbHandles mbWorkDir distPref bt
    postBuildHook
      | Just postBuild <- postBuildComponentHook $ buildHooks hooks =
          postBuild
      | otherwise =
          const $ return ()
    pkgDescr = localPkgDescr lbi
    bt = PD.buildType pkgDescr
    mbWorkDir = mbWorkDirLBI lbi
    distPref = Cabal.fromFlag $ Cabal.buildDistPref flags
#endif

--------------------------------------------------------------------------------
-- Haddock

haddock
  :: VerbosityHandles
  -> Cabal.HaddockFlags
  -> LocalBuildInfo
  -> [String]
  -> IO [MonitorFilePath]
#ifdef __MHS__
haddock _ _ _ _ =
  error "Distribution.Client.InLibrary.haddock: in-library setup is not supported under MHS"
#else
haddock verbHandles flags lbi _args =
  haddock_setupHooks preBuildHook verbHandles pkgDescr lbi Cabal.knownSuffixHandlers flags
  where
    preBuildHook = ExternalHooksExe.buildTypePreBuildHooks verbHandles mbWorkDir distPref bt
    pkgDescr = localPkgDescr lbi
    bt = PD.buildType pkgDescr
    mbWorkDir = mbWorkDirLBI lbi
    distPref = Cabal.fromFlag $ Cabal.haddockDistPref flags
#endif

--------------------------------------------------------------------------------
-- Repl

repl
  :: VerbosityHandles
  -> Cabal.ReplFlags
  -> LocalBuildInfo
  -> [String]
  -> IO [MonitorFilePath]
#ifdef __MHS__
repl _ _ _ _ =
  error "Distribution.Client.InLibrary.repl: in-library setup is not supported under MHS"
#else
repl verbHandles flags lbi _args =
  repl_setupHooks preBuildHook verbHandles pkgDescr lbi flags Cabal.knownSuffixHandlers []
  where
    preBuildHook = ExternalHooksExe.buildTypePreBuildHooks verbHandles mbWorkDir distPref bt
    pkgDescr = localPkgDescr lbi
    bt = PD.buildType pkgDescr
    mbWorkDir = mbWorkDirLBI lbi
    distPref = Cabal.fromFlag $ Cabal.replDistPref flags
#endif

--------------------------------------------------------------------------------
-- Copy

copy
  :: VerbosityHandles
  -> Cabal.CopyFlags
  -> LocalBuildInfo
  -> [String]
  -> IO ()
#ifdef __MHS__
copy _ _ _ _ =
  error "Distribution.Client.InLibrary.copy: in-library setup is not supported under MHS"
#else
copy verbHandles flags lbi _args =
  install_setupHooks hooks verbHandles pkgDescr lbi flags
  where
    verb = mkVerbosity verbHandles $ Cabal.fromFlag $ Cabal.copyVerbosity flags
    hooks = installHooks $ ExternalHooksExe.buildTypeSetupHooks verb mbWorkDir distPref bt
    pkgDescr = localPkgDescr lbi
    bt = PD.buildType pkgDescr
    mbWorkDir = mbWorkDirLBI lbi
    distPref = Cabal.fromFlag $ Cabal.copyDistPref flags
#endif

--------------------------------------------------------------------------------
-- Test, bench, register.
--
-- NB: no hooks into these phases.

test
  :: VerbosityHandles
  -> Cabal.TestFlags
  -> LocalBuildInfo
  -> [String]
  -> IO ()
#ifdef __MHS__
test _ _ _ _ =
  error "Distribution.Client.InLibrary.test: in-library setup is not supported under MHS"
#else
test verb flags lbi args =
  Cabal.test args verb pkgDescr lbi flags
  where
    pkgDescr = localPkgDescr lbi
#endif

bench
  :: VerbosityHandles
  -> Cabal.BenchmarkFlags
  -> LocalBuildInfo
  -> [String]
  -> IO ()
bench verb flags lbi args =
#ifdef __MHS__
  error "Distribution.Client.InLibrary.bench: in-library setup is not supported under MHS"
#else
  Cabal.bench args verb pkgDescr lbi flags
#endif
  where
    pkgDescr = localPkgDescr lbi

register
  :: Cabal.RegisterFlags
  -> LocalBuildInfo
  -> [String]
  -> IO ()
#ifdef __MHS__
register _ _ _ =
  error "Distribution.Client.InLibrary.register: in-library setup is not supported under MHS"
#else
register flags lbi _args = Cabal.register pkgDescr lbi flags
  where
    pkgDescr = localPkgDescr lbi
#endif
