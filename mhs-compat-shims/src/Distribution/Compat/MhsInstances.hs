{-# LANGUAGE CPP #-}

-- | MicroHs compatibility instances.
--
-- MicroHs\'s @containers@ package gates its 'Data.Data' instances
-- behind @__GLASGOW_HASKELL__@, so they are missing under MicroHs.
-- This module provides those instances as orphans, for use behind
-- @__MHS__@ only (GHC already gets them from @containers@ itself).
--
-- The method bodies are intentionally absent: MicroHs accepts empty
-- instance bodies, and nothing in this repository calls 'Data'
-- methods on these types. The instances exist only so that
-- @deriving@ clauses and explicit @instance@ contexts resolve.
module Distribution.Compat.MhsInstances
  (
  ) where

#ifdef __MHS__
import Data.Data (Data)
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

instance (Data a, Ord a) => Data (Set.Set a)

instance (Data k, Data v, Ord k) => Data (Map.Map k v)

instance Data T.Text

instance Data BS.ByteString
#endif
