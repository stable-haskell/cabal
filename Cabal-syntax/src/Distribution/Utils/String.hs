module Distribution.Utils.String
  ( trim
  ) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)

-- @since 3.8.0.0
trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace
