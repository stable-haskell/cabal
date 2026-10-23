{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}

module ProfileTarget (checkTarget) where

import Foreign.C.Types
import TargetLayout

foreign import ccall "profiling_enabled" profilingEnabled :: IO CInt

checkTarget :: IO Bool
checkTarget = do
  enabled <- profilingEnabled
  return (enabled == 1 && layoutProfiling == 1 && $([|6 * 7 :: Int|]) == 42)
