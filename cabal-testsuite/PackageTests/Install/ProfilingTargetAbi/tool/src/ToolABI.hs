{-# LANGUAGE ForeignFunctionInterface #-}

module ToolABI (isProfiled) where

import Foreign.C.Types

foreign import ccall "tool_is_profiled" toolIsProfiled :: IO CInt

isProfiled :: IO Bool
isProfiled = (/= 0) <$> toolIsProfiled
