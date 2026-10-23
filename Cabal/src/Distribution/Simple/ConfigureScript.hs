{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
-- Module      :  Distribution.Simple.ConfigureScript
-- Copyright   :  Isaac Jones 2003-2005
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
module Distribution.Simple.ConfigureScript
  ( runConfigureScript
  , runConfigureScriptWithCompiler
  , runConfigureScriptWithCompilerAndProgramDb
  ) where

import Distribution.Compat.Prelude
import Prelude ()

-- local

import qualified Data.Map as Map
import Distribution.PackageDescription
import Distribution.Pretty
import Distribution.Simple.Compiler (Compiler, compilerProperties)
import Distribution.Simple.Configure (findDistPrefOrDefault)
import Distribution.Simple.Errors
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Program.Db (prependProgramSearchPath)
import Distribution.Simple.Setup.Common
import Distribution.Simple.Setup.Config
import Distribution.Simple.Utils
import Distribution.System (OS (..), Platform (..), buildPlatform)
import Distribution.Utils.NubList
import Distribution.Utils.Path
import Distribution.Verbosity

-- Base
import Distribution.Compat.Environment (getEnvironment)
import Distribution.Compat.GetShortPathName (getShortPathName)
import System.Directory (createDirectoryIfMissing, doesFileExist, makeAbsolute)
#ifdef mingw32_HOST_OS
import System.FilePath    (normalise, splitDrive)
import qualified System.FilePath as FilePath
#endif

runConfigureScript
  :: VerbosityHandles
  -> ConfigFlags
  -> FlagAssignment
  -> Platform
  -- ^ host platform
  -> IO ()
runConfigureScript verbHandles cfg flags hp = do
  runConfigureScriptWithCompiler Nothing verbHandles cfg flags hp

-- | Run the configure script using the selected compiler's full target
-- platform metadata when available. The older API remains available above and
-- falls back to the historical Cabal platform rendering.
runConfigureScriptWithCompiler
  :: Maybe Compiler
  -> VerbosityHandles
  -> ConfigFlags
  -> FlagAssignment
  -> Platform
  -> IO ()
runConfigureScriptWithCompiler mbCompiler verbHandles cfg flags hp = do
  let commonCfg = configCommonFlags cfg
      verbosity = mkVerbosity verbHandles (fromFlag $ setupVerbosity commonCfg)
      extraPath = fromNubList $ configProgramPathExtra cfg
  programDb <- prependProgramSearchPath verbosity extraPath [] $ addKnownProgram gppProgram defaultProgramDb
  let programDb' =
        userSpecifyArgss (configProgramArgs cfg)
          . userSpecifyPaths (configProgramPaths cfg)
          $ programDb
  runConfigureScriptWithCompilerAndProgramDb
    mbCompiler
    programDb'
    verbHandles
    cfg
    flags
    hp

-- | Run a configure script with Cabal's selected C and C++ tools.
runConfigureScriptWithCompilerAndProgramDb
  :: Maybe Compiler
  -> ProgramDb
  -> VerbosityHandles
  -> ConfigFlags
  -> FlagAssignment
  -> Platform
  -> IO ()
runConfigureScriptWithCompilerAndProgramDb mbCompiler programDb verbHandles cfg flags hp = do
  let commonCfg = configCommonFlags cfg
      verbosity = mkVerbosity verbHandles (fromFlag $ setupVerbosity commonCfg)

  dist_dir <- findDistPrefOrDefault $ setupDistPref commonCfg

  let build_dir = dist_dir </> makeRelativePathEx "build"
      mbWorkDir = flagToMaybe $ setupWorkingDir commonCfg
      build_in = interpretSymbolicPath mbWorkDir build_dir

  let configureScriptPath = packageRoot commonCfg </> "configure"

  confExists <- doesFileExist configureScriptPath
  unless confExists $
    dieWithException verbosity (ConfigureScriptNotFound configureScriptPath)

  let args = configureScriptArgs mbCompiler hp (platformToTriple buildPlatform) (configureArgs cfg)
  validateTargetArg verbosity "--host" args
  validateTargetArg verbosity "--build" args

  configureFile <- toUnix <$> makeAbsolute configureScriptPath

  env <- getEnvironment
  (ccProg, ccFlags) <- configureCCompiler verbosity programDb
  ccProgShort <- getShortPathName ccProg
  (mcxxProgShort, mcxxFlags) <- do
    mprog <- needProgram verbosity gppProgram programDb
    case mprog of
      Just (p, _) -> do
        let pInv = programInvocation p []
        cxxProgShort <- getShortPathName $ progInvokePath pInv
        return (Just cxxProgShort, Just $ progInvokeArgs pInv)
      Nothing -> return (Nothing, Nothing)

  let mkFlagsEnv fs var = maybe (unwords fs) (++ (" " ++ unwords fs)) (lookup var env)
      compilerEnv =
        [("CC", Just ccProgShort), ("CFLAGS", Just $ mkFlagsEnv ccFlags "CFLAGS")]
          ++ [ ("CXX", Just cxxProgShort)
             | Just cxxProgShort <- [mcxxProgShort]
             ]
          ++ [ ("CXXFLAGS", Just $ mkFlagsEnv cxxFlags "CXXFLAGS")
             | Just cxxFlags <- [mcxxFlags]
             ]
      compilerArgs =
        ["CC=" ++ ccProgShort]
          ++ ["CXX=" ++ cxxProgShort | Just cxxProgShort <- [mcxxProgShort]]

  let args' = configureFile : args ++ compilerArgs

  createDirectoryIfMissing True build_in
  withExtraPathEnv verbosity extraPath $
    withEnvOverrides verbosity (cabalFlagMap ++ cabalFlagEnv ++ progEnv ++ compilerEnv) $ do
      runProgramInvocation verbosity $
        -- We call `sh configure` rather than `configure` because on Windows you
        -- cannot run a shell script (there is no such thing as she-bang)
        (simpleProgramInvocation "sh" args')
          { progInvokeCwd = Just build_in
          }
  where
    -- Convert a flag name to name of environment variable to represent its
    -- value for the configure script.
    flagEnvVar :: FlagName -> String
    flagEnvVar flag = "CABAL_FLAG_" ++ map f (unFlagName flag)
      where
        f c
          | isAlphaNum c = c
          | otherwise = '_'

    -- A map from such env vars to every flag name and value where the name
    -- name maps to that that env var.
    cabalFlagMap =
      [ (flagEnvVar flag, Just $ if bool then "1" else "0")
      | (flag, bool) <- unFlagAssignment flags
      ]

    cabalFlagEnv =
      [
        ( "CABAL_FLAGS"
        , Just $ unwords [showFlagValue fv | fv <- unFlagAssignment flags]
        )
      ]

    progEnv =
      [
        ( "GHC"
        , canonicalisePathSeparator <$> lookup "ghc" (configProgramPaths cfg)
        )
      ,
        ( "GHC_PKG"
        , canonicalisePathSeparator <$> lookup "ghc-pkg" (configProgramPaths cfg)
        )
      ]

    extraPath = fromNubList $ configProgramPathExtra cfg

configureScriptArgs :: Maybe Compiler -> Platform -> String -> [String] -> [String]
configureScriptArgs mbCompiler selectedPlatform buildTriple userArgs =
  userArgs ++ configureTargetDefaults hostTriple buildTriple userArgs
  where
    hostTriple = fromMaybe (platformToTriple selectedPlatform) $ do
      selectedCompiler <- mbCompiler
      Map.lookup "Target platform" (compilerProperties selectedCompiler)

validateTargetArg :: Verbosity -> String -> [String] -> IO ()
validateTargetArg verbosity option args = do
  for_ (configureTargetValues option args) $ \target ->
    when (null target || "--" `isPrefixOf` target) $
      die' verbosity $
        "Autoconf " ++ option ++ " requires a machine target platform value."
  for_ (configureTargetValues option args) $ rejectProfilingTarget verbosity option

configureTargetValues :: String -> [String] -> [String]
configureTargetValues option = go
  where
    go [] = []
    go (arg : rest)
      | arg == option = case rest of
          value : more -> value : go more
          [] -> [""]
      | (option ++ "=") `isPrefixOf` arg =
          drop (length option + 1) arg : go rest
      | otherwise = go rest

rejectProfilingTarget :: Verbosity -> String -> String -> IO ()
rejectProfilingTarget verbosity option target =
  when ("-prof" `isSuffixOf` target) $
    die' verbosity $
      "Autoconf "
        ++ option
        ++ " must name the machine target platform, not the logical "
        ++ "profiling target name "
        ++ target
        ++ ". Use "
        ++ option
        ++ "=<machine> and "
        ++ "select the profiling compiler with --with-compiler=<machine>-prof-ghc."

platformToTriple :: Platform -> String
platformToTriple (Platform arch os) = prettyShow arch <> "-" <> fromOS os
  where
    fromOS Linux = "unknown-linux"
    fromOS Windows = "unknown-mingw32"
    fromOS OSX = "apple-darwin"
    fromOS FreeBSD = "unknown-freebsd"
    fromOS OpenBSD = "unknown-openbsd"
    fromOS NetBSD = "unknown-netbsd"
    fromOS DragonFly = "unknown-dragonflybsd"
    fromOS Solaris = "sun-solaris"
    fromOS AIX = "ibm-aix"
    fromOS HPUX = "hp-hpux"
    fromOS IRIX = "sgi-irix"
    fromOS HaLVM = "unknown-halvm"
    fromOS Hurd = "unknown-gnu"
    fromOS IOS = "apple-ios"
    fromOS Android = "unknown-linux-android"
    fromOS Ghcjs = "unknown-ghcjs"
    fromOS Wasi = "unknown-wasi"
    fromOS Haiku = "unknown-haiku"
    fromOS (OtherOS otheros) = "unknown-" <> otheros

-- | Convert Windows path to Unix ones
-- Julian: it is incomplete
toUnix :: String -> String
#ifdef mingw32_HOST_OS
toUnix s = let tmp = normalise s
               (l, rest) = case splitDrive tmp of
                             ([],  x) -> ("/"      , x)
                             (h:_, x) -> ('/':h:"/", x)
               parts = FilePath.splitDirectories rest
           in  l ++ intercalate "/" parts
#else
toUnix s = s
#endif
