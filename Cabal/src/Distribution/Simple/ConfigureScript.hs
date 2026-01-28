{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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
  ) where

import Distribution.Compat.Prelude
import Prelude ()

-- local
import Distribution.PackageDescription
import Distribution.Pretty
import Distribution.Simple.Configure (findDistPrefOrDefault)
import Distribution.Simple.Errors
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Setup.Common
import Distribution.Simple.Setup.Config
import Distribution.Simple.Utils
import Distribution.System (Platform, buildPlatform)
import Distribution.Utils.NubList
import Distribution.Utils.Path

-- Base
import System.Directory (createDirectoryIfMissing, doesFileExist)
import qualified System.FilePath as FilePath
#ifdef mingw32_HOST_OS
import System.FilePath    (normalise, splitDrive)
#endif
import Distribution.Compat.Directory (makeAbsolute)

runConfigureScript
  :: ConfigFlags
  -> FlagAssignment
  -> Platform
  -- ^ host platform
  -> IO ()
runConfigureScript cfg flags hp = do
  let commonCfg = configCommonFlags cfg
      verbosity = fromFlag $ setupVerbosity commonCfg

  dist_dir <- findDistPrefOrDefault $ setupDistPref commonCfg

  let build_dir = dist_dir </> makeRelativePathEx "build"
      mbWorkDir = flagToMaybe $ setupWorkingDir commonCfg
      build_in = interpretSymbolicPath mbWorkDir build_dir

  let configureScriptPath = packageRoot commonCfg </> "configure"
  confExists <- doesFileExist configureScriptPath
  unless confExists $
    dieWithException verbosity (ConfigureScriptNotFound configureScriptPath)
  configureFile <- toUnix <$> makeAbsolute configureScriptPath

  createDirectoryIfMissing True build_in
  withExtraPathEnv verbosity extraPath $
    withEnvOverrides verbosity envOverrides $ do
      logInvoke verbosity configureFile args
      runProgramInvocation verbosity $
        (simpleProgramInvocation configureFile args)
          { progInvokeCwd = Just build_in
          }

  where
      cc = foldMap ("CC=" <>) $ lookup "gcc" (configProgramPaths cfg)
      cc_flags = foldMap (("CC_FLAGS=" <>) . unwords) $ lookup "gcc" (configProgramArgs cfg)
      cxx = foldMap ("CXX=" <>) $ lookup "gpp" (configProgramPaths cfg)
      cxx_flags = foldMap (("CXX_FLAGS=" <>) . unwords) $ lookup "gpp" (configProgramArgs cfg)
      ghc = foldMap ("GHC=" <>) $ lookup "ghc" (configProgramPaths cfg)
      ghc_pkg = foldMap ("GHC_PKG=" <>) $ lookup "ghc-pkg" (configProgramPaths cfg)

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
          [( "CABAL_FLAGS"
           , Just $ unwords [showFlagValue fv | fv <- unFlagAssignment flags]
           )]

      extraPath = fromNubList $ configProgramPathExtra cfg
      envOverrides = cabalFlagMap ++ cabalFlagEnv
      maybeHostFlag = ["--host=" ++ show (pretty hp) | hp /= buildPlatform]
      args = configureArgs cfg ++ [cc, cc_flags, cxx, cxx_flags, ghc, ghc_pkg] ++ maybeHostFlag

-- | Convert Windows path to Unix ones
toUnix :: String -> String
#ifdef mingw32_HOST_OS
toUnix s = let tmp = normalise s
               (l, rest) = case splitDrive tmp of
                             ([],  x) -> ("/"      , x)
                             (h:_, x) -> ('/':h:"/", x)
               parts = FilePath.splitDirectories rest
           in  l ++ intercalate "/" parts
#else
toUnix s = intercalate "/" $ FilePath.splitDirectories s
#endif
