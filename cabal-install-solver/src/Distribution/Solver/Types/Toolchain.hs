{-# LANGUAGE DeriveGeneric #-}

module Distribution.Solver.Types.Toolchain
  ( Toolchain (..)
  , Toolchains
  , Stage (..)
  , Staged (..)
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Simple.Compiler
import Distribution.Simple.Program.Db
import Distribution.Solver.Types.Stage (getStage, Stage (..), Staged (..))
import Distribution.System

---------------------------
-- Toolchain
--

data Toolchain = Toolchain
  { toolchainPlatform :: Platform
  , toolchainCompiler :: Compiler
  , toolchainProgramDb :: ProgramDb
  , toolchainPackageDBs :: PackageDBStackCWD
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
