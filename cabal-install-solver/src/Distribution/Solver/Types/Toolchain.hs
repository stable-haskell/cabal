{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Distribution.Solver.Types.Toolchain
  ( Toolchain (..)
  , Toolchains (..)
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Simple.Compiler
import Distribution.Simple.Program.Db
import Distribution.Solver.Types.Stage (Stage (..), Staged)
import Distribution.System
import Data.Maybe (fromJust)
import Distribution.Utils.Structured (Structured(..))

---------------------------
-- Toolchain
--

data Toolchain = Toolchain
  { toolchainPlatform :: Platform
  , toolchainCompiler :: Compiler
  , toolchainProgramDb :: ProgramDb
  }
  deriving (Show, Generic, Typeable)

-- TODO: review this
instance Eq Toolchain where
  lhs == rhs =
    (((==) `on` toolchainPlatform) lhs rhs)
      && (((==) `on` toolchainCompiler) lhs rhs)
      && ((((==)) `on` (configuredPrograms . toolchainProgramDb)) lhs rhs)

instance Binary Toolchain
instance Structured Toolchain

type Toolchains = Staged Toolchain

-- data Toolchains = Toolchains
--   { getToolchain :: Stage -> Toolchain
--   }
--   deriving (Generic, Typeable)

-- instance Eq Toolchains where
--   lhs == rhs =
--     all
--       (\stage -> getToolchain lhs stage == getToolchain rhs stage)
--       [minBound .. maxBound]

-- instance Show Toolchains where
--   showsPrec _ toolchains =
--     showList
--       [ (stage, getToolchain toolchains stage)
--       | stage <- [minBound .. maxBound]
--       ]

-- -- toolchainFor :: Stage -> Toolchains -> Toolchain
-- -- toolchainFor Build = buildToolchain
-- -- toolchainFor Host = hostToolchain

-- instance Binary Toolchains where
--   put toolchains = put (tabulate toolchains)
--   -- TODO this could be done better
--   get = do
--     t <- get
--     return (Toolchains (\s -> fromJust (lookup s t))) 
    
-- -- TODO: I have no idea if this is right
-- instance Structured Toolchains where
--   structure _ = structure (Proxy :: Proxy [(Stage, Toolchain)])

-- tabulate :: Toolchains -> [(Stage, Toolchain)]
-- tabulate toolchains =
--   [ (stage, getToolchain toolchains stage)
--   | stage <- [minBound .. maxBound]
--   ]
