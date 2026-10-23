module Main where

import Control.Monad (unless, when)
import System.Directory (copyFile)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import ToolABI (isProfiled)

main :: IO ()
main = do
  profiled <- isProfiled
  args <- getArgs
  case args of
    [] -> do
      unless profiled $ fail "installed tool must use the profiling ABI"
      putStrLn "installed profiling tool"
    [_, input, output] -> do
      when profiled $ fail "install preprocessor must use the ordinary ABI"
      hPutStrLn stderr "ordinary install preprocessor"
      copyFile input output
    _ -> fail "unexpected preprocessor arguments"
