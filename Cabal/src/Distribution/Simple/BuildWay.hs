{-# LANGUAGE LambdaCase #-}

module Distribution.Simple.BuildWay
  ( BuildWay (..)
  , buildWayObjectExtension
  , buildWayInterfaceExtension
  ) where

-- | Artifact ways. Profiling belongs to the compiler's target ABI, so it
-- does not introduce another object, interface, or library filename.
data BuildWay = StaticWay | DynWay
  deriving (Eq, Ord, Show, Read, Enum)

-- | Returns the object extension for the given build way (e.g. "dyn_o" for 'DynWay' on ELF)
buildWayObjectExtension :: String -> BuildWay -> String
buildWayObjectExtension ext = \case
  StaticWay -> ext
  DynWay -> "dyn_" ++ ext

buildWayInterfaceExtension :: BuildWay -> String
buildWayInterfaceExtension = buildWayObjectExtension "hi"
