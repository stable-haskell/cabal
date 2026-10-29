module Main where

import ProfileTarget

main :: IO ()
main = do
  ok <- checkTarget
  if ok then putStrLn "profiling target ABI" else fail "target C ABI mismatch"
