module Distribution.Client.Types.ReadyPackage
  ( GenericReadyPackage (..)
  , ReadyPackage
  ) where

import Distribution.Client.Types.ConfiguredPackage (ConfiguredPackage)
import Distribution.Client.Types.PackageLocation (UnresolvedPkgLoc)
import Distribution.Client.Types.GenericReadyPackage (GenericReadyPackage(..))

type ReadyPackage = GenericReadyPackage (ConfiguredPackage UnresolvedPkgLoc)
