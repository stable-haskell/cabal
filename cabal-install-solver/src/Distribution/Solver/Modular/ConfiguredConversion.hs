module Distribution.Solver.Modular.ConfiguredConversion
    ( convCP
    ) where

import Data.Maybe
import Prelude hiding (pi)
import Data.Either (partitionEithers)

import qualified Distribution.InstalledPackageInfo as IPI
import qualified Distribution.Simple.PackageIndex as SI
import Distribution.Types.LibraryName (LibraryName(..))

import Distribution.Solver.Modular.Configured
import Distribution.Solver.Modular.Package

import           Distribution.Solver.Types.ComponentDeps (ComponentDeps)
import qualified Distribution.Solver.Types.PackageIndex as CI
import           Distribution.Solver.Types.PackagePath
import           Distribution.Solver.Types.ResolverPackage
import           Distribution.Solver.Types.SolverId
import           Distribution.Solver.Types.SolverPackage
import           Distribution.Solver.Types.InstSolverPackage
import           Distribution.Solver.Types.SourcePackage
import           Distribution.Solver.Types.Stage (Staged (..))

-- | Converts from the solver specific result @CP QPN@ into
-- a 'ResolverPackage', which can then be converted into
-- the install plan.
convCP :: Staged SI.InstalledPackageIndex ->
          CI.PackageIndex (SourcePackage loc) ->
          CP QPN -> ResolverPackage loc
convCP iidx sidx (CP qpi fa es ds) =
  case qpi of
    -- Installed
    (PI qpn (I s _ (Inst pi)))  ->
      let idx = getStage iidx s
          ipi = fromMaybe (error "convCP: lookupUnitId failed") $ SI.lookupUnitId idx pi
          -- Installed sub-libraries of the chosen instance (main-lib
          -- instances only).  Must mirror the sibling enumeration in
          -- 'convIP', which advertised these components to the solver;
          -- elaboration surfaces them as plan nodes so a `pkg:sublib`
          -- dep edge resolves to the sub-library's unit id (via
          -- mkCCMapping / toConfiguredComponent).
          subs = case IPI.sourceLibName ipi of
            LSubLibName _ -> []
            LMainLibName ->
              [ sib
              | ((spid, LSubLibName _), sibs) <- SI.allPackagesBySourcePackageIdAndLibName idx
              , spid == IPI.sourcePackageId ipi
              , sib <- take 1 sibs ]
      in
      PreExisting $
                  InstSolverPackage {
                    instSolverStage = s,
                    instSolverQPN = qpn,
                    instSolverPkgIPI = ipi,
                    instSolverPkgSubLibs = subs,
                    instSolverPkgLibDeps = fmap fst ds',
                    instSolverPkgExeDeps = fmap snd ds'
                  }
    -- "In repo" i.e. a source package
    (PI qpn@(Q _path pn) (I s v (InRepo _pn))) ->
      let pi = PackageIdentifier pn v in
      Configured $
                  SolverPackage {
                      solverPkgStage = s,
                      solverPkgQPN = qpn,
                      solverPkgSource = fromMaybe (error "convCP: lookupPackageId failed") $ CI.lookupPackageId sidx pi,
                      solverPkgFlags = fa,
                      solverPkgStanzas = es,
                      solverPkgLibDeps = fmap fst ds',
                      solverPkgExeDeps = fmap snd ds'
                    }
  where
    ds' :: ComponentDeps ([SolverId] {- lib -}, [SolverId] {- exe -})
    ds' = fmap (partitionEithers . map convConfId) ds

convConfId :: PI QPN -> Either SolverId {- is lib -} SolverId {- is exe -}
convConfId (PI (Q (PackagePath _stage q) pn) (I stage v loc)) =
    case loc of
        Inst pi -> Left (PreExistingId stage sourceId pi)
        _otherwise
          | QualExe _ pn' <- q
          -- NB: the dependencies of the executable are also
          -- qualified.  So the way to tell if this is an executable
          -- dependency is to make sure the qualifier is pointing
          -- at the actual thing.  Fortunately for us, I was
          -- silly and didn't allow arbitrarily nested build-tools
          -- dependencies, so a shallow check works.
          , pn == pn' -> Right (PlannedId stage sourceId)
          | otherwise    -> Left  (PlannedId stage sourceId)
  where
    sourceId    = PackageIdentifier pn v
