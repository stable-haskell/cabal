{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveTraversable #-}

module Distribution.Solver.Types.Stage
  ( Stage (..)
  , Staged (..)
  , tabulate
  , index
  , foldMapWithKey
  ) where

import Data.Maybe (fromJust)
import Distribution.Compat.Prelude
import Distribution.Utils.Structured (Structured (..))
import Prelude ()
import GHC.Stack

data Stage
  = -- | -- The system where the build is running
    Build
  | -- | -- The system where the built artifacts will run
    Host
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Generic, Typeable)

instance Binary Stage
instance Structured Stage

-- TOOD: I think there is similar code for stanzas, compare.

newtype Staged a = Staged
  { getStage :: Stage -> a
  }
  deriving (Functor, Generic, Typeable)
  deriving Applicative via ((->) Stage)
  
instance Eq a => Eq (Staged a) where
  lhs == rhs =
    all
      (\stage -> getStage lhs stage == getStage rhs stage)
      [minBound .. maxBound]

instance Show a => Show (Staged a) where
  showsPrec _ staged =
    showList
      [ (stage, getStage staged stage)
      | stage <- [minBound .. maxBound]
      ]

instance Foldable Staged where
  foldMap f (Staged gs) = foldMap (f . gs) [minBound..maxBound]

instance Traversable Staged where
  traverse f = fmap index . traverse (traverse f) . tabulate

instance Binary a => Binary (Staged a) where
  put staged = put (tabulate staged)
  -- TODO this could be done better I think
  get =  index <$> get

-- TODO: I have no idea if this is right
instance (Typeable a, Structured a) => Structured (Staged a) where
  structure _ = structure (Proxy :: Proxy [(Stage, a)])

tabulate :: Staged a -> [(Stage, a)]
tabulate staged =
  [ (stage, getStage staged stage)
  | stage <- [minBound .. maxBound]
  ]

index :: HasCallStack => [(Stage, a)] -> Staged a
index t = Staged (\s -> fromJust (lookup s t))

foldMapWithKey :: Monoid m => (Stage -> a -> m) -> Staged a -> m
foldMapWithKey f = foldMap (uncurry f) . tabulate
