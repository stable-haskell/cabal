{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Distribution.Solver.Types.SolverPackage
    ( SolverPackage(..)
    , dumpSolverPackage
    ) where

import Distribution.Solver.Compat.Prelude
import Prelude ()

import Distribution.Package ( Package(..) )
import Distribution.PackageDescription ( FlagAssignment, showFlagAssignment, nullFlagAssignment )
import Distribution.Pretty (Pretty(..))
import Distribution.Solver.Types.ComponentDeps ( ComponentDeps )
import qualified Distribution.Solver.Types.ComponentDeps as CD
import Distribution.Solver.Types.OptionalStanza
import Distribution.Solver.Types.SolverId
import Distribution.Solver.Types.SourcePackage
import Distribution.Solver.Types.Stage ( Stage )
import Distribution.Solver.Types.PackagePath ( QPN, showQPN )
import Text.PrettyPrint hiding ((<>))

-- | A 'SolverPackage' is a package specified by the dependency solver.
-- It will get elaborated into a 'ConfiguredPackage' or even an
-- 'ElaboratedConfiguredPackage'.
--
-- NB: 'SolverPackage's are essentially always with 'UnresolvedPkgLoc',
-- but for symmetry we have the parameter.  (Maybe it can be removed.)
--
data SolverPackage loc = SolverPackage {
        solverPkgStage   :: Stage,
        solverPkgQPN     :: QPN,
        solverPkgSource  :: SourcePackage loc,
        solverPkgFlags   :: FlagAssignment,
        solverPkgStanzas :: OptionalStanzaSet,
        solverPkgLibDeps :: ComponentDeps [SolverId],
        solverPkgExeDeps :: ComponentDeps [SolverId]
    }
  deriving (Eq, Show, Generic)

instance Binary loc => Binary (SolverPackage loc)
instance Structured loc => Structured (SolverPackage loc)

instance Package (SolverPackage loc) where
  packageId = packageId . solverPkgSource

dumpSolverPackage :: SolverPackage loc -> Doc
dumpSolverPackage SolverPackage {..} =
  vcat
    [ hang (text "qpn:") 2 $
        text (showQPN solverPkgQPN) <+> parens (text "source" <+> pretty solverPkgStage <+> pretty (srcpkgPackageId solverPkgSource))
    , vcat
        [ hang (text "flags:") 2 $ text (showFlagAssignment solverPkgFlags)
        | not (nullFlagAssignment solverPkgFlags)
        ]
    , vcat
        [ hang (text "stanzas:") 2 $ text (showStanzas solverPkgStanzas)
        | not (optStanzaSetNull solverPkgStanzas)
        ]
    , hang (text "components:") 2 $ vcat
        [ hang (pretty comp) 2 $ vcat
          [ vcat [ hang (text "lib-deps:") 2 (vcat (map pretty libDeps)) | not (null libDeps) ]
          , vcat [ hang (text "exe-deps:") 2 (vcat (map pretty exeDeps)) | not (null exeDeps) ]
          ]
        | (comp, (libDeps, exeDeps)) <- CD.toList (CD.zip solverPkgLibDeps solverPkgExeDeps)
        ]
    ]