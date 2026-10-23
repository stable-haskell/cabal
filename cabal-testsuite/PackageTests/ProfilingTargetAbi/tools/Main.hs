{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Control.Monad (when)
import Foreign.C.Types
import System.Environment

foreign import ccall "host_profiling_enabled" hostProfilingEnabled :: IO CInt

main :: IO ()
main = do
  enabled <- hostProfilingEnabled
  when (enabled /= 0) $ fail "build tool must use the ordinary ABI"
  args <- getArgs
  case args of
    [_, input, output] -> readFile input >>= writeFile output
    _ -> fail "expected GHC preprocessor arguments"
