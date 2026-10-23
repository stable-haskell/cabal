module MyLib (someFunc, Async) where

import Control.Concurrent.Async

someFunc :: IO (Async ())
someFunc = async (return ())
