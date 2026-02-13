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
import Distribution.System (Platform(..), OS(..), buildPlatform)
import Distribution.Utils.NubList
import Distribution.Utils.Path

-- Base
import System.Directory (createDirectoryIfMissing, doesFileExist)
#ifdef mingw32_HOST_OS
import System.FilePath    (normalise, splitDrive)
import qualified System.FilePath as FilePath
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

  putStrLn $ "[runConfigureScript] commonCfg= " ++ show commonCfg
  let configureScriptPath = packageRoot commonCfg </> "configure"

  putStrLn $ "[runConfigureScript] configureScriptPath = " ++ configureScriptPath

  confExists <- doesFileExist configureScriptPath
  unless confExists $
    dieWithException verbosity (ConfigureScriptNotFound configureScriptPath)

  configureFile <- toUnix <$> makeAbsolute configureScriptPath
  putStrLn $ "[runConfigureScript] configureFile = " ++ configureFile
  putStrLn $ "[runConfigureScript] arg = " ++ unwords args

  createDirectoryIfMissing True build_in
  withExtraPathEnv verbosity extraPath $
    withEnvOverrides verbosity envOverrides $ do
      runProgramInvocation verbosity $
        -- We call `sh configure` rather than `configure` because on Windows you
        -- cannot run a shell script (there is no such thing as she-bang)
        (simpleProgramInvocation "sh" (configureFile : args))
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
          [( "CABAL_FLAGS"
           , Just $ unwords [showFlagValue fv | fv <- unFlagAssignment flags]
           )]

      progEnv =
          [( "CC"
            , canonicalisePathSeparator <$> lookup "gcc" (configProgramPaths cfg)
           ),
           ( "CXX"
           , canonicalisePathSeparator <$> lookup "gpp" (configProgramPaths cfg)
           ),
           ( "GHC"
           , canonicalisePathSeparator <$> lookup "ghc" (configProgramPaths cfg)
           ),
           ( "GHC_PKG"
           , canonicalisePathSeparator <$> lookup "ghc-pkg" (configProgramPaths cfg)
           )
          ]

      extraPath = fromNubList $ configProgramPathExtra cfg
      envOverrides = cabalFlagMap ++ cabalFlagEnv ++ progEnv
      args =
        configureArgs cfg ++ ["--host=" <> platformToTriple hp, "--build=" <> platformToTriple buildPlatform]


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
