module Nested (nestedFunc, Async) where

import Control.Concurrent.Async

nestedFunc :: IO (Async ())
nestedFunc = async (return ())
