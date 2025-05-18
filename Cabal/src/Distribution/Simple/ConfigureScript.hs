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
import Distribution.Simple.Program.Db
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
import Distribution.Compat.Environment (getEnvironment)
import Distribution.Compat.GetShortPathName (getShortPathName)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map

import Control.Concurrent.MVar
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

-- Global state for locks, one MVar per build directory path.
-- The MVar () acts as a mutex: it's full when the lock is available (unlocked),
-- and empty when taken (locked).
{-# NOINLINE configureLocks #-}
configureLocks :: IORef (Map.Map FilePath (MVar ()))
configureLocks = unsafePerformIO (newIORef Map.empty)

-- Gets or creates a lock for a given path.
-- Ensures that only one MVar is associated with each path.
getOrCreateLock :: FilePath -> IO (MVar ())
getOrCreateLock path = do
    -- Optimistically create a new MVar. It will be used if the path is not already in the map.
    -- This MVar is created "full" (i.e., containing ()), meaning the lock is initially available.
    newLock <- newMVar ()
    atomicModifyIORef' configureLocks $ \currentMap ->
        case Map.lookup path currentMap of
            Just existingLock -> (currentMap, existingLock) -- Lock already exists, return it. The newLock we created is discarded.
            Nothing           -> (Map.insert path newLock currentMap, newLock) -- New lock inserted, return it.

runConfigureScript
  :: ConfigFlags
  -> FlagAssignment
  -> ProgramDb
  -> Platform
  -- ^ host platform
  -> IO ()
runConfigureScript cfg flags programDb hp = do
  let commonCfg = configCommonFlags cfg
      verbosity = fromFlag $ setupVerbosity commonCfg
  dist_dir <- findDistPrefOrDefault $ setupDistPref commonCfg
  let build_dir = dist_dir </> makeRelativePathEx "build"
      mbWorkDir = flagToMaybe $ setupWorkingDir commonCfg
      configureScriptPath = packageRoot commonCfg </> "configure"
  confExists <- doesFileExist configureScriptPath
  unless confExists $
    dieWithException verbosity (ConfigureScriptNotFound configureScriptPath)
  configureFile <-
    makeAbsolute $ configureScriptPath
  env <- getEnvironment
  (ccProg, ccFlags) <- configureCCompiler verbosity programDb
  ccProgShort <- getShortPathName ccProg
  -- The C compiler's compilation and linker flags (e.g.
  -- "C compiler flags" and "Gcc Linker flags" from GHC) have already
  -- been merged into ccFlags, so we set both CFLAGS and LDFLAGS
  -- to ccFlags
  -- We don't try and tell configure which ld to use, as we don't have
  -- a way to pass its flags too

  let configureFile' = toUnix configureFile
  -- autoconf is fussy about filenames, and has a set of forbidden
  -- characters that can't appear in the build directory, etc:
  -- https://www.gnu.org/software/autoconf/manual/autoconf.html#File-System-Conventions
  --
  -- This has caused hard-to-debug failures in the past (#5368), so we
  -- detect some cases early and warn with a clear message. Windows's
  -- use of backslashes is problematic here, so we'll switch to
  -- slashes, but we do still want to fail on backslashes in POSIX
  -- paths.
  --
  -- TODO: We don't check for colons, tildes or leading dashes. We
  -- also should check the builddir's path, destdir, and all other
  -- paths as well.
  for_ badAutoconfCharacters $ \(c, cname) ->
    when (c `elem` FilePath.dropDrive configureFile') $
      warn verbosity $
        concat
          [ "The path to the './configure' script, '"
          , configureFile'
          , "', contains the character '"
          , [c]
          , "' ("
          , cname
          , ")."
          , " This may cause the script to fail with an obscure error, or for"
          , " building the package to fail later."
          ]

  let
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
    cabalFlagMap :: Map String (NonEmpty (FlagName, Bool))
    cabalFlagMap =
      Map.fromListWith
        (<>)
        [ (flagEnvVar flag, (flag, bool) :| [])
        | (flag, bool) <- unFlagAssignment flags
        ]
  -- A map from env vars to flag names to the single flag we will go with
  cabalFlagMapDeconflicted :: Map String (FlagName, Bool) <-
    flip Map.traverseWithKey cabalFlagMap $ \envVar -> \case
      -- No conflict: no problem
      singleFlag :| [] -> pure singleFlag
      -- Conflict: warn and discard all but first
      collidingFlags@(firstFlag :| _ : _) -> do
        let quote s = "'" ++ s ++ "'"
            toName = quote . unFlagName . fst
            renderedList = intercalate ", " $ NonEmpty.toList $ toName <$> collidingFlags
        warn verbosity $
          unwords
            [ "Flags"
            , renderedList
            , "all map to the same environment variable"
            , quote envVar
            , "causing a collision."
            , "The value first flag"
            , toName firstFlag
            , "will be used."
            ]
        pure firstFlag

  let cabalFlagEnv =
        [ (envVar, Just val)
        | (envVar, (_, bool)) <- Map.toList cabalFlagMapDeconflicted
        , let val = if bool then "1" else "0"
        ]
          ++ [
               ( "CABAL_FLAGS"
               , Just $ unwords [showFlagValue fv | fv <- unFlagAssignment flags]
               )
             ]
  let extraPath = fromNubList $ configProgramPathExtra cfg
  let cflagsEnv =
        maybe (unwords ccFlags) (++ (" " ++ unwords ccFlags)) $
          lookup "CFLAGS" env
      spSep = [FilePath.searchPathSeparator]
      pathEnv =
        maybe
          (intercalate spSep extraPath)
          ((intercalate spSep extraPath ++ spSep) ++)
          $ lookup "PATH" env
      overEnv =
        ("CFLAGS", Just cflagsEnv)
          : [("PATH", Just pathEnv) | not (null extraPath)]
          ++ cabalFlagEnv
      maybeHostFlag = if hp == buildPlatform then [] else ["--host=" ++ show (pretty hp)]
      args' = configureFile' : args ++ ["CC=" ++ ccProgShort] ++ maybeHostFlag
      shProg = simpleProgram "sh"
  progDb <- prependProgramSearchPath verbosity extraPath [] emptyProgramDb
  shConfiguredProg <-
    lookupProgram shProg
      `fmap` configureProgram verbosity shProg progDb

  -- Acquire the lock specific to this build_dir
  -- build_dir is used as the key for the lock.
  configureLock <- getOrCreateLock configureFile'

  let runLockedConfigureAction = do
        case shConfiguredProg of
          Just sh -> do
            let build_in = interpretSymbolicPath mbWorkDir build_dir
            createDirectoryIfMissing True build_in
            warn verbosity $ "Configure lock acquired. Running configure script in " ++ build_in
            runProgramInvocation verbosity $
              (programInvocation (sh{programOverrideEnv = overEnv}) args')
                { progInvokeCwd = Just build_in
                }
          Nothing -> dieWithException verbosity NotFoundMsg

  warn verbosity $ "Attempting to acquire configure lock for " ++ configureFile'
  -- withMVar takes the MVar (blocks if already taken), runs the action,
  -- and ensures the MVar is put back, even if the action throws an exception.
  withMVar configureLock $ \() ->
    -- The '()' means the MVar holds a unit value; we're interested in its full/empty state.
    runLockedConfigureAction
  warn verbosity $ "Configure lock released for " ++ configureFile'

  where
    args = configureArgs backwardsCompatHack cfg
    backwardsCompatHack = False

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

badAutoconfCharacters :: [(Char, String)]
badAutoconfCharacters =
  [ (' ', "space")
  , ('\t', "tab")
  , ('\n', "newline")
  , ('\0', "null")
  , ('"', "double quote")
  , ('#', "hash")
  , ('$', "dollar sign")
  , ('&', "ampersand")
  , ('\'', "single quote")
  , ('(', "left bracket")
  , (')', "right bracket")
  , ('*', "star")
  , (';', "semicolon")
  , ('<', "less-than sign")
  , ('=', "equals sign")
  , ('>', "greater-than sign")
  , ('?', "question mark")
  , ('[', "left square bracket")
  , ('\\', "backslash")
  , ('`', "backtick")
  , ('|', "pipe")
  ]
