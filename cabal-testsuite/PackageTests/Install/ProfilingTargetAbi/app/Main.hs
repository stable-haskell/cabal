module Main where

import Control.Monad (unless)
import ToolABI (isProfiled)

main :: IO ()
main = do
  profiled <- isProfiled
  unless profiled $ fail "installed application must use the profiling ABI"
  putStrLn "installed profiling application"
