{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module exposes functions to build and register packages.
module Distribution.Client.ProjectBuilding.UnpackedPackage
  ( buildAndInstallUnpackedPackage
  , PackageBuildingPhase(..)

    -- ** Utilities
  , annotateFailure
  , annotateFailureNoLog
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.ProjectBuilding.Types
import Distribution.Client.ProjectConfig
import Distribution.Client.ProjectConfig.Types
import Distribution.Client.ProjectPlanning
import Distribution.Client.ProjectPlanning.Types

import Distribution.Client.DistDirLayout
import Distribution.Client.JobControl
import Distribution.Client.Setup
  ( CommonSetupFlags
  , filterBenchmarkFlags
  , filterBuildFlags
  , filterConfigureFlags
  , filterCopyFlags
  , filterHaddockArgs
  , filterHaddockFlags
  , filterRegisterFlags
  , filterReplFlags
  , filterTestFlags
  )
import Distribution.Client.SetupWrapper
import Distribution.Client.Types hiding
  ( BuildFailure (..)
  , BuildOutcome
  , BuildOutcomes
  , BuildResult (..)
  )
import Distribution.Client.Utils
  ( ProgressPhase (..)
  , progressMessage
  )

import Distribution.Compat.Lens
import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import qualified Distribution.InstalledPackageInfo as Installed
import Distribution.Package
import Distribution.Simple.Command (CommandUI)
import Distribution.Simple.Compiler
  ( PackageDBStackCWD
  , coercePackageDBStack
  )
import Distribution.Simple.LocalBuildInfo
  ( ComponentName (..)
  , LibraryName (..)
  )
import Distribution.Simple.Program
import qualified Distribution.Simple.Register as Cabal
import qualified Distribution.Simple.Setup as Cabal

import Distribution.Types.PackageDescription.Lens (componentModules)

import Distribution.Simple.Utils
import Distribution.Utils.Path hiding
  ( (<.>)
  , (</>)
  )
import Distribution.Version

import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE

import Control.Exception (Handler (..), SomeAsyncException, catches, onException)
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesFileExist, removeFile)
import System.FilePath (takeDirectory, (</>))
import System.IO (Handle, IOMode (AppendMode), withFile)
import System.Semaphore (SemaphoreName (..))


import Distribution.Client.Errors

import qualified Distribution.Compat.Graph as Graph
import Distribution.Client.ProjectBuilding.PackageFileMonitor
import Distribution.PackageDescription (BuildType(..))
import qualified Distribution.PackageDescription as PD
import Distribution.Client.FileMonitor (monitorFileHashed, beginUpdateFileMonitor)
import Distribution.Client.SourceFiles (needElaboratedConfiguredPackage)
import Distribution.Client.SrcDist (allPackageSourceFiles)
import Distribution.Client.RebuildMonad (execRebuild)

-- | Each unpacked package is processed in the following phases:
--
-- * Configure phase
-- * Build phase
-- * Haddock phase
-- * Install phase (copy + register)
-- * Register phase
-- * Test phase
-- * Bench phase
-- * Repl phase
--
-- Depending on whether we are installing the package or building it inplace,
-- the phases will be carried out differently. For example, when installing,
-- the test, benchmark, and repl phase are ignored.
data PackageBuildingPhase
  = PBConfigurePhase {runConfigure :: IO ()}
  | PBBuildPhase {runBuild :: IO ()}
  | PBHaddockPhase {runHaddock :: IO ()}
  | PBInstallPhase
      { runCopy :: Cabal.CopyDest -> IO ()
      , runRegister
          :: PackageDBStackCWD
          -> Cabal.RegisterOptions
          -> IO InstalledPackageInfo
      }
  | PBTestPhase {runTest :: IO ()}
  | PBBenchPhase {runBench :: IO ()}
  | PBReplPhase {runRepl :: IO ()}

-- | Structures the phases of building and registering a package amongst others
-- (see t'PackageBuildingPhase'). Delegates logic specific to a certain
-- building style (notably, inplace vs install) to the delegate function that
-- receives as an argument t'PackageBuildingPhase')
buildAndRegisterUnpackedPackage
  :: Verbosity
  -> DistDirLayout
  -> Maybe SemaphoreName
  -- ^ Whether to pass a semaphore to build process
  -- this is different to BuildTimeSettings because the
  -- name of the semaphore is created freshly each time.
  -> BuildTimeSettings
  -> Lock
  -> Lock
  -> ElaboratedSharedConfig
  -> ElaboratedReadyPackage
  -> SymbolicPath CWD (Dir Pkg)
  -> SymbolicPath Pkg (Dir Dist)
  -> Maybe FilePath
  -- ^ The path to an /initialized/ log file
  -> (PackageBuildingPhase -> IO ())
  -> IO ()
buildAndRegisterUnpackedPackage
  verbosity
  DistDirLayout{distTempDirectory}
  maybe_semaphore
  buildTimeSettings@BuildTimeSettings{buildSettingNumJobs, buildSettingKeepTempFiles}
  registerLock
  cacheLock
  pkgshared
  rpkg@(ReadyPackage pkg)
  srcdir
  builddir
  mlogFile
  delegate = do
    info verbosity $ "\n\nbuildAndRegisterUnpackedPackage: " ++ prettyShow (Graph.nodeKey pkg)
    -- Configure phase
    delegate $
      PBConfigurePhase $
        annotateFailure mlogFile ConfigureFailed $ do
          info verbosity $ "--- Configure phase " ++ prettyShow (Graph.nodeKey pkg)
          setup configureCommand Cabal.configCommonFlags configureFlags configureArgs

    -- Build phase
    delegate $
      PBBuildPhase $
        annotateFailure mlogFile BuildFailed $ do
          info verbosity $ "--- Build phase " ++ prettyShow (Graph.nodeKey pkg)
          setup buildCommand Cabal.buildCommonFlags (return . buildFlags) buildArgs

    -- Haddock phase
    whenHaddock $
      delegate $
        PBHaddockPhase $
          annotateFailure mlogFile HaddocksFailed $ do
            info verbosity $ "--- Haddock phase " ++ prettyShow (Graph.nodeKey pkg)
            setup haddockCommand Cabal.haddockCommonFlags (return . haddockFlags) haddockArgs

    -- Install phase
    delegate $
      PBInstallPhase
        { runCopy = \destdir ->
            annotateFailure mlogFile InstallFailed $ do
              info verbosity $ "--- Install phase (copy) " ++ prettyShow (Graph.nodeKey pkg)
              setup Cabal.copyCommand Cabal.copyCommonFlags (return . copyFlags destdir) copyArgs
        , runRegister = \pkgDBStack registerOpts ->
            annotateFailure mlogFile InstallFailed $ do
              info verbosity $ "--- Install phase (register) " ++ prettyShow (Graph.nodeKey pkg)
              -- We register ourselves rather than via Setup.hs. We need to
              -- grab and modify the InstalledPackageInfo. We decide what
              -- the installed package id is, not the build system.
              ipkg0 <- generateInstalledPackageInfo
              let ipkg = ipkg0{Installed.installedUnitId = uid}
              criticalSection registerLock $
                Cabal.registerPackage
                  verbosity
                  toolchainCompiler
                  toolchainProgramDb
                  Nothing
                  (coercePackageDBStack pkgDBStack)
                  ipkg
                  registerOpts
              return ipkg
        }

    -- Test phase
    whenTest $
      delegate $
        PBTestPhase $
          annotateFailure mlogFile TestsFailed $ do
            info verbosity $ "--- Test phase " ++ prettyShow (Graph.nodeKey pkg)
            setup testCommand Cabal.testCommonFlags (return . testFlags) testArgs

    -- Bench phase
    whenBench $
      delegate $
        PBBenchPhase $
          annotateFailure mlogFile BenchFailed $ do
            info verbosity $ "--- Benchmark phase " ++ prettyShow (Graph.nodeKey pkg)
            setup benchCommand Cabal.benchmarkCommonFlags (return . benchFlags) benchArgs

    -- Repl phase
    whenRepl $
      delegate $
        PBReplPhase $
          annotateFailure mlogFile ReplFailed $ do
            info verbosity $ "--- Repl phase " ++ prettyShow (Graph.nodeKey pkg)
            setupInteractive replCommand Cabal.replCommonFlags replFlags replArgs

    return ()
    where
      uid = installedUnitId rpkg

      Toolchain{toolchainCompiler, toolchainProgramDb} = elabToolchain pkg

      comp_par_strat = case maybe_semaphore of
        Just sem_name -> Cabal.toFlag (getSemaphoreName sem_name)
        _ -> Cabal.NoFlag

      whenTest action
        | null (elabTestTargets pkg) = return ()
        | otherwise = action

      whenBench action
        | null (elabBenchTargets pkg) = return ()
        | otherwise = action

      whenRepl action
        | null (elabReplTarget pkg) = return ()
        | otherwise = action

      whenHaddock action
        | hasValidHaddockTargets pkg = action
        | otherwise = return ()

      mbWorkDir = useWorkingDir scriptOptions
      commonFlags = setupHsCommonFlags verbosity mbWorkDir builddir buildSettingKeepTempFiles

      configureCommand = Cabal.configureCommand defaultProgramDb
      configureFlags v =
        flip filterConfigureFlags v
          <$> setupHsConfigureFlags
            (\p -> makeSymbolicPath <$> canonicalizePath p)
            rpkg
            commonFlags
      configureArgs _ = setupHsConfigureArgs pkg

      buildCommand = Cabal.buildCommand defaultProgramDb
      buildFlags v =
        flip filterBuildFlags v $
          setupHsBuildFlags
            comp_par_strat
            pkg
            pkgshared
            commonFlags
      buildArgs _ = setupHsBuildArgs pkg

      copyFlags destdir v =
        flip filterCopyFlags v $
          setupHsCopyFlags
            pkg
            pkgshared
            commonFlags
            destdir
      -- In theory, we could want to copy less things than those that were
      -- built, but instead, we simply copy the targets that were built.
      copyArgs = buildArgs

      testCommand = Cabal.testCommand -- defaultProgramDb
      testFlags v =
        flip filterTestFlags v $
          setupHsTestFlags
            pkg
            commonFlags
      testArgs _ = setupHsTestArgs pkg

      benchCommand = Cabal.benchmarkCommand
      benchFlags v =
        flip filterBenchmarkFlags v $
          setupHsBenchFlags
            pkg
            pkgshared
            commonFlags
      benchArgs _ = setupHsBenchArgs pkg

      replCommand = Cabal.replCommand defaultProgramDb
      replFlags v =
        flip filterReplFlags v $
          setupHsReplFlags
            pkg
            pkgshared
            commonFlags
      replArgs _ = setupHsReplArgs pkg

      haddockCommand = Cabal.haddockCommand
      haddockFlags v =
        flip filterHaddockFlags v $
          setupHsHaddockFlags
            pkg
            buildTimeSettings
            commonFlags
      haddockArgs v =
        flip filterHaddockArgs v $
          setupHsHaddockArgs pkg

      scriptOptions =
        setupHsScriptOptions
          rpkg
          pkgshared
          srcdir
          builddir
          (isParallelBuild buildSettingNumJobs)
          cacheLock

      setup
        :: CommandUI flags
        -> (flags -> CommonSetupFlags)
        -> (Version -> IO flags)
        -> (Version -> [String])
        -> IO ()
      setup cmd getCommonFlags flags args =
        withLogging $ \mLogFileHandle -> do
          let opts = scriptOptions{useLoggingHandle = mLogFileHandle}
          setupWrapper
            verbosity
            opts
            (Just (elabPkgDescription pkg))
            cmd
            getCommonFlags
            flags
            args

      setupInteractive
        :: CommandUI flags
        -> (flags -> CommonSetupFlags)
        -> (Version -> flags)
        -> (Version -> [String])
        -> IO ()
      setupInteractive cmd getCommonFlags flags args =
        setupWrapper
          verbosity
          scriptOptions { isInteractive = True }
          (Just (elabPkgDescription pkg))
          cmd
          getCommonFlags
          (\v -> return (flags v))
          args

      generateInstalledPackageInfo :: IO InstalledPackageInfo
      generateInstalledPackageInfo =
        withTempInstalledPackageInfoFile
          verbosity
          distTempDirectory
          $ \pkgConfDest -> do
            let registerFlags v =
                  flip filterRegisterFlags v $
                    setupHsRegisterFlags
                      pkg
                      pkgshared
                      commonFlags
                      pkgConfDest
            setup (Cabal.registerCommand) Cabal.registerCommonFlags (\v -> return (registerFlags v)) (const [])

      withLogging :: (Maybe Handle -> IO r) -> IO r
      withLogging action =
        case mlogFile of
          Nothing -> action Nothing
          Just logFile -> withFile logFile AppendMode (action . Just)


buildAndInstallUnpackedPackage
  :: Verbosity
  -> DistDirLayout
  -> Maybe SemaphoreName
  -- ^ Whether to pass a semaphore to build process
  -- this is different to BuildTimeSettings because the
  -- name of the semaphore is created freshly each time.
  -> BuildTimeSettings
  -> Lock
  -> Lock
  -> ElaboratedInstallPlan
  -> ElaboratedSharedConfig
  -> ElaboratedReadyPackage
  -> BuildStatusRebuild
  -> SymbolicPath CWD (Dir Pkg)
  -> SymbolicPath Pkg (Dir Dist)
  -> IO BuildResult
buildAndInstallUnpackedPackage
  verbosity
  distDirLayout
  maybe_semaphore
  buildSettings@BuildTimeSettings{buildSettingNumJobs, buildSettingLogFile}
  registerLock
  cacheLock
  plan
  pkgshared
  rpkg@(ReadyPackage pkg)
  buildStatus
  srcdir
  builddir = do
    createDirectoryIfMissingVerbose verbosity True (interpretSymbolicPath (Just srcdir) builddir)
    createDirectoryIfMissingVerbose verbosity True (distPackageCacheDirectory distDirLayout dparams)

    -- TODO: [code cleanup] deal consistently with talking to older
    --      Setup.hs versions, much like we do for ghc, with a proper
    --      options type and rendering step which will also let us
    --      call directly into the lib, rather than always going via
    --      the lib's command line interface, which would also allow
    --      passing data like installed packages, compiler, and
    --      program db for a quicker configure.

    -- TODO: [required feature] docs and tests
    -- TODO: [required feature] sudo re-exec

    initLogFile

    let docsResult = DocsNotTried
        testsResult = TestsNotTried
        buildResult :: BuildResultMisc
        buildResult = (docsResult, testsResult)

    buildAndRegisterUnpackedPackage
      verbosity
      distDirLayout
      maybe_semaphore
      buildSettings
      registerLock
      cacheLock
      pkgshared
      rpkg
      srcdir
      builddir
      mlogFile
      $ \case
        PBConfigurePhase{runConfigure} -> do
          whenReconfigure $ do
            noticeProgress ProgressStarting
            runConfigure
            invalidatePackageRegFileMonitor packageFileMonitor
            updatePackageConfigFileMonitor packageFileMonitor (getSymbolicPath srcdir) pkg
        PBBuildPhase{runBuild} -> do
          whenRebuild $ do
            noticeProgress ProgressBuilding
            timestamp <- beginUpdateFileMonitor
            runBuild
            -- Be sure to invalidate the cache if building throws an exception!
            -- If not, we'll abort execution with a stale recompilation cache.
            -- See ghc#24926 for an example of how this can go wrong.
              `onException` invalidatePackageRegFileMonitor packageFileMonitor            

            let listSimple =
                  execRebuild (getSymbolicPath srcdir) (needElaboratedConfiguredPackage pkg)
                listSdist =
                  fmap (map monitorFileHashed) $
                    allPackageSourceFiles verbosity (getSymbolicPath srcdir)
                ifNullThen m m' = do
                  xs <- m
                  if null xs then m' else return xs

            monitors <- case PD.buildType (elabPkgDescription pkg) of
              Simple -> listSimple
              -- If a Custom setup was used, AND the Cabal is recent
              -- enough to have sdist --list-sources, use that to
              -- determine the files that we need to track.  This can
              -- cause unnecessary rebuilding (for example, if README
              -- is edited, we will try to rebuild) but there isn't
              -- a more accurate Custom interface we can use to get
              -- this info.  We prefer not to use listSimple here
              -- as it can miss extra source files that are considered
              -- by the Custom setup.
              _
                | elabSetupScriptCliVersion pkg >= mkVersion [1, 17] ->
                    -- However, sometimes sdist --list-sources will fail
                    -- and return an empty list.  In that case, fall
                    -- back on the (inaccurate) simple tracking.
                    listSdist `ifNullThen` listSimple
                | otherwise ->
                    listSimple
            
            let dep_monitors =
                  map monitorFileHashed $
                    elabInplaceDependencyBuildCacheFiles
                      distDirLayout
                      plan
                      pkg

            updatePackageBuildFileMonitor
              packageFileMonitor
              (getSymbolicPath srcdir)
              timestamp
              pkg
              buildStatus
              (monitors ++ dep_monitors)
              buildResult

        PBHaddockPhase{runHaddock} -> do
          noticeProgress ProgressHaddock
          runHaddock
        PBInstallPhase{runCopy, runRegister} -> do
          -- NOTE: We re-copy and re-register if we rebuild. It seems to make sense.
          whenRebuild $ do
            noticeProgress ProgressInstalling

            runCopy (Cabal.CopyToDb (elabRegistrationPackageDb pkg))

            if elabRequiresRegistration pkg then do
              ipi <- runRegister
                  (elabRegisterPackageDBStack pkg)
                  Cabal.defaultRegisterOptions
                    { Cabal.registerMultiInstance = True
                    , Cabal.registerSuppressFilesCheck = True
                    }
              updatePackageRegFileMonitor packageFileMonitor (getSymbolicPath srcdir) (Just ipi)
            else
              info verbosity $ "registerPkg: elab does NOT require registration for " ++ prettyShow uid

        PBTestPhase{runTest} -> runTest
        PBBenchPhase{runBench} -> runBench
        PBReplPhase{runRepl} -> runRepl

    -- TODO: [nice to have] we currently rely on Setup.hs copy to do the right
    -- thing. Although we do copy into an image dir and do the move into the
    -- final location ourselves, perhaps we ought to do some sanity checks on
    -- the image dir first.

    -- TODO: [required eventually] note that for nix-style
    -- installations it is not necessary to do the
    -- 'withWin32SelfUpgrade' dance, but it would be necessary for a
    -- shared bin dir.

    noticeProgress ProgressCompleted

    return
      BuildResult
        { buildResultDocs = docsResult
        , buildResultTests = testsResult
        , buildResultLogFile = mlogFile
        }
    where
      uid = installedUnitId rpkg
      pkgid = packageId rpkg

      dparams = elabDistDirParams pkg
      packageFileMonitor = newPackageFileMonitor distDirLayout dparams

      whenReconfigure action = case buildStatus of
        BuildStatusConfigure _ -> action
        _ -> return ()

      whenRebuild action
        | null (elabBuildTargets pkg)
        , -- NB: we have to build the test/bench suite!
          null (elabTestTargets pkg)
        , null (elabBenchTargets pkg) =
            return ()
        | otherwise = action

      dispname :: String
      dispname = case elabPkgOrComp pkg of
        -- Packages built altogether, instead of per component
        ElabPackage ElaboratedPackage{pkgWhyNotPerComponent} ->
          prettyShow pkgid
            ++ " (all, legacy fallback: "
            ++ unwords (map whyNotPerComponent $ NE.toList pkgWhyNotPerComponent)
            ++ ")"
        -- Packages built per component
        ElabComponent comp ->
          prettyShow pkgid
            ++ " ("
            ++ maybe "custom" prettyShow (compComponentName comp)
            ++ ")"

      noticeProgress :: ProgressPhase -> IO ()
      noticeProgress phase =
        when (isParallelBuild buildSettingNumJobs) $
          progressMessage verbosity phase dispname

      mlogFile :: Maybe FilePath
      mlogFile =
        case buildSettingLogFile of
          Nothing -> Nothing
          -- TODO: we ignore mkLogFile, because that would require us to fix the templating
          -- and add support for '$stage'. Part of the templating is in Cabal (the library),
          -- so doing that properly might require some refactoring and careful thought.
          --
          -- It also means we introduce a regression here and the user can't really overwrite
          -- the log destination anymore.
          --
          -- Last but not least, removing the @Nothing -> Nothing@ case would trigger a bug on
          -- FreeBSD, because 'getSetupMethod' would always pick the 'SelfExecMethod', which
          -- then on FreeBSD would try to execute @_build/cabal/bin/cabal@ and fail, because
          -- it just changed the CWD to a source directory. This needs further investigation.
          --
          -- An alternative patch was constructed here: https://github.com/stable-haskell/cabal/commit/604cab98c9a599953c7d95e4f37af449d5357577
          Just _ -> Just
            $ distDirectory distDirLayout
            </> "logs"
            </> prettyShow (elabStage pkg)
            </> betterPlatform (elabToolchain pkg)
            </> prettyShow (elabUnitId pkg)

      initLogFile :: IO ()
      initLogFile =
        case mlogFile of
          Nothing -> return ()
          Just logFile -> do
            createDirectoryIfMissing True (takeDirectory logFile)
            exists <- doesFileExist logFile
            when exists $ removeFile logFile


--------------------------------------------------------------------------------
-- * Exported Utils
--------------------------------------------------------------------------------

{- FOURMOLU_DISABLE -}
annotateFailureNoLog :: (SomeException -> BuildFailureReason)
                     -> IO a -> IO a
annotateFailureNoLog annotate action =
  annotateFailure Nothing annotate action

annotateFailure :: Maybe FilePath
                -> (SomeException -> BuildFailureReason)
                -> IO a -> IO a
annotateFailure mlogFile annotate action =
  action `catches`
    -- It's not just IOException and ExitCode we have to deal with, there's
    -- lots, including exceptions from the hackage-security and tar packages.
    -- So we take the strategy of catching everything except async exceptions.
    [
#if MIN_VERSION_base(4,7,0)
      Handler $ \async -> throwIO (async :: SomeAsyncException)
#else
      Handler $ \async -> throwIO (async :: AsyncException)
#endif
    , Handler $ \other -> handler (other :: SomeException)
    ]
  where
    handler :: Exception e => e -> IO a
    handler = throwIO . BuildFailure mlogFile . annotate . toException

--------------------------------------------------------------------------------
-- * Other Utils
--------------------------------------------------------------------------------

hasValidHaddockTargets :: ElaboratedConfiguredPackage -> Bool
hasValidHaddockTargets ElaboratedConfiguredPackage{..}
  | not elabBuildHaddocks = False
  | otherwise = any componentHasHaddocks components
  where
    components :: [ComponentTarget]
    components =
      elabBuildTargets
        ++ elabTestTargets
        ++ elabBenchTargets
        ++ elabReplTarget
        ++ elabHaddockTargets

    componentHasHaddocks :: ComponentTarget -> Bool
    componentHasHaddocks (ComponentTarget name _) =
      case name of
        CLibName LMainLibName -> hasHaddocks
        CLibName (LSubLibName _) -> elabHaddockInternal && hasHaddocks
        CFLibName _ -> elabHaddockForeignLibs && hasHaddocks
        CExeName _ -> elabHaddockExecutables && hasHaddocks
        CTestName _ -> elabHaddockTestSuites && hasHaddocks
        CBenchName _ -> elabHaddockBenchmarks && hasHaddocks
      where
        hasHaddocks = not (null (elabPkgDescription ^. componentModules name))

withTempInstalledPackageInfoFile
  :: Verbosity
  -> FilePath
  -> (FilePath -> IO ())
  -> IO InstalledPackageInfo
withTempInstalledPackageInfoFile verbosity tempdir action =
  withTempDirectory verbosity tempdir "package-registration-" $ \dir -> do
    -- make absolute since @action@ will often change directory
    abs_dir <- canonicalizePath dir

    let pkgConfDest = abs_dir </> "pkgConf"
    action pkgConfDest

    readPkgConf "." pkgConfDest
  where
    pkgConfParseFailed :: String -> IO a
    pkgConfParseFailed perror =
      dieWithException verbosity $ PkgConfParseFailed perror

    readPkgConf :: FilePath -> FilePath -> IO InstalledPackageInfo
    readPkgConf pkgConfDir pkgConfFile = do
      pkgConfStr <- BS.readFile (pkgConfDir </> pkgConfFile)
      (warns, ipkg) <- case Installed.parseInstalledPackageInfo pkgConfStr of
        Left perrors -> pkgConfParseFailed $ unlines $ NE.toList perrors
        Right (warns, ipkg) -> return (warns, ipkg)

      unless (null warns) $
        warn verbosity $
          unlines warns

      return ipkg

