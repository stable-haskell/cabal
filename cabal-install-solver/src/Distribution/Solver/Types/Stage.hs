{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}

module Distribution.Solver.Types.Stage
  ( Stage (..)
  , Staged (..)
  , tabulate
  , index
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
  deriving (Eq, Read, Show, Enum, Bounded, Generic, Typeable)

instance Binary Stage
instance Structured Stage

-- TOOD: I think there is similar code for stanzas, compare.

data Staged a = Staged
  { getStage :: Stage -> a
  }
  deriving (Functor, Generic, Typeable)

instance Eq a => Eq (Staged a) where
  lhs == rhs =
    all
      (\stage -> getStage lhs stage == getStage rhs stage)
      [minBound .. maxBound]

instance Show a => Show (Staged a) where
  showsPrec _ toolchains =
    showList
      [ (stage, getStage toolchains stage)
      | stage <- [minBound .. maxBound]
      ]

instance Binary a => Binary (Staged a) where
  put toolchains = put (tabulate toolchains)
  -- TODO this could be done better I think
  get =  index <$> get

-- TODO: I have no idea if this is right
instance (Typeable a, Structured a) => Structured (Staged a) where
  structure _ = structure (Proxy :: Proxy [(Stage, a)])

tabulate :: Staged a -> [(Stage, a)]
tabulate toolchains =
  [ (stage, getStage toolchains stage)
  | stage <- [minBound .. maxBound]
  ]

index :: HasCallStack => [(Stage, a)] -> Staged a
index t = Staged (\s -> fromJust (lookup s t))
