{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}

module Distribution.Compat.Environment (getEnvironment, lookupEnv, setEnv, unsetEnv)
where

import Distribution.Compat.Prelude
import Prelude ()

import System.Environment (lookupEnv, unsetEnv)
import qualified System.Environment as System
#ifdef __MHS__
-- MicroHS provides no 'System.Environment.Blank'; its
-- 'System.Environment.setEnv' always overwrites, like 'Blank.setEnv' with
-- the overwrite flag set, so we use it directly.
import System.Environment (setEnv)
#else
import qualified System.Environment.Blank as Blank (setEnv)
#endif

getEnvironment :: IO [(String, String)]
#ifdef mingw32_HOST_OS
-- On Windows, the names of environment variables are case-insensitive, but are
-- often given in mixed-case (e.g. "PATH" is "Path"), so we have to normalise
-- them.
getEnvironment = fmap upcaseVars System.getEnvironment
  where
    upcaseVars = map upcaseVar
    upcaseVar (var, val) = (map toUpper var, val)
#else
getEnvironment = System.getEnvironment
#endif

-- | @setEnv name value@ sets the specified environment variable to @value@.
setEnv :: String -> String -> IO ()
#ifdef __MHS__
setEnv = System.setEnv
#else
setEnv key value = Blank.setEnv key value True
#endif
