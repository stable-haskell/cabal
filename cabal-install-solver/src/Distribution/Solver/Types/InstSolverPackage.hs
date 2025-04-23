{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Distribution.Solver.Types.InstSolverPackage
    ( InstSolverPackage(..)
    , dumpInstSolverPackage
    ) where

import Distribution.Solver.Compat.Prelude
import Prelude ()

import Distribution.Package ( Package(..), HasMungedPackageId(..), HasUnitId(..) )
import Distribution.Pretty (Pretty(..))
import Distribution.Solver.Types.ComponentDeps ( ComponentDeps )
import qualified Distribution.Solver.Types.ComponentDeps as CD
import Distribution.Solver.Types.PackagePath (QPN, showQPN)
import Distribution.Solver.Types.SolverId
import Distribution.Solver.Types.Stage (Stage)
import Distribution.Types.MungedPackageId
import Distribution.Types.PackageId
import Distribution.Types.MungedPackageName
import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import Text.PrettyPrint hiding ((<>))

-- | An 'InstSolverPackage' is a pre-existing installed package
-- specified by the dependency solver.
data InstSolverPackage = InstSolverPackage {
      instSolverStage :: Stage,
      instSolverQPN :: QPN,
      instSolverPkgIPI :: InstalledPackageInfo,
      instSolverPkgLibDeps :: ComponentDeps [SolverId],
      instSolverPkgExeDeps :: ComponentDeps [SolverId]
    }
  deriving (Eq, Show, Generic)

instance Binary InstSolverPackage
instance Structured InstSolverPackage

instance Package InstSolverPackage where
    packageId i =
        -- HACK! See Note [Index conversion with internal libraries]
        let MungedPackageId mpn v = mungedId i
        in PackageIdentifier (encodeCompatPackageName mpn) v

instance HasMungedPackageId InstSolverPackage where
    mungedId = mungedId . instSolverPkgIPI

instance HasUnitId InstSolverPackage where
    installedUnitId = installedUnitId . instSolverPkgIPI

dumpInstSolverPackage :: InstSolverPackage -> Doc
dumpInstSolverPackage InstSolverPackage {..} = 
    vcat
        [ hang (text "qpn: ") 2 $
            text (showQPN instSolverQPN) <+> parens (text "installed" <+> pretty instSolverStage <+> pretty (installedUnitId instSolverPkgIPI))
        , hang (text "components:") 2 $
            vcat
                [ hang (pretty comp) 2 $ vcat
                    [ vcat [ hang (text "lib-deps:") 2 (vcat (map pretty libDeps)) | not (null libDeps) ]
                    , vcat [ hang (text "exe-deps:") 2 (vcat (map pretty exeDeps)) | not (null exeDeps) ]
                    ]
                | (comp, (libDeps, exeDeps)) <- CD.toList (CD.zip instSolverPkgLibDeps instSolverPkgExeDeps)
                ]
        ]