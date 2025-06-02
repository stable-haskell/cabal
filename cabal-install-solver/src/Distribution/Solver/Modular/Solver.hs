module Distribution.Solver.Modular.Solver
    ( SolverConfig(..)
    , solve
    , PruneAfterFirstSuccess(..)
    ) where

import Distribution.Solver.Compat.Prelude
import Prelude ()

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import Distribution.Verbosity

import Distribution.Compiler (CompilerInfo)

import Distribution.Solver.Types.PackagePath
import Distribution.Solver.Types.PackagePreferences
import Distribution.Solver.Types.PkgConfigDb (PkgConfigDb)
import Distribution.Solver.Types.LabeledPackageConstraint
import Distribution.Solver.Types.Settings
import Distribution.Solver.Types.Variable

import Distribution.Solver.Modular.Assignment
import Distribution.Solver.Modular.Builder
import Distribution.Solver.Modular.Cycles
import Distribution.Solver.Modular.Dependency
import Distribution.Solver.Modular.Explore
import Distribution.Solver.Modular.Index
import Distribution.Solver.Modular.Log
import Distribution.Solver.Modular.Message
import Distribution.Solver.Modular.Package
import qualified Distribution.Solver.Modular.Preference as P
import Distribution.Solver.Modular.Validate
import Distribution.Solver.Modular.Linking
import Distribution.Solver.Modular.PSQ (PSQ)
import Distribution.Solver.Modular.RetryLog
import Distribution.Solver.Modular.Tree
import qualified Distribution.Solver.Modular.PSQ as PSQ

import Distribution.Simple.Setup (BooleanFlag(..))
import Distribution.Solver.Types.Stage (Staged, Stage(..))


-- | Various options for the modular solver.
data SolverConfig = SolverConfig {
  reorderGoals           :: ReorderGoals,
  countConflicts         :: CountConflicts,
  fineGrainedConflicts   :: FineGrainedConflicts,
  minimizeConflictSet    :: MinimizeConflictSet,
  avoidReinstalls        :: AvoidReinstalls,
  shadowPkgs             :: ShadowPkgs,
  strongFlags            :: StrongFlags,
  onlyConstrained        :: OnlyConstrained,
  maxBackjumps           :: Maybe Int,
  enableBackjumping      :: EnableBackjumping,
  solveExecutables       :: SolveExecutables,
  goalOrder              :: Maybe (Variable QPN -> Variable QPN -> Ordering),
  solverVerbosity        :: Verbosity,
  pruneAfterFirstSuccess :: PruneAfterFirstSuccess
}

-- | Whether to remove all choices after the first successful choice at each
-- level in the search tree.
newtype PruneAfterFirstSuccess = PruneAfterFirstSuccess Bool

-- | Run all solver phases.
--
-- In principle, we have a valid tree after 'validationPhase', which
-- means that every 'Done' node should correspond to valid solution.
--
-- There is one exception, though, and that is cycle detection, which
-- has been added relatively recently. Cycles are only removed directly
-- before exploration.
--
solve :: SolverConfig                         -- ^ solver parameters
      -> Staged CompilerInfo
      -> Staged (Maybe PkgConfigDb)
      -> Index                                -- ^ all available packages as an index
      -> (PN -> PackagePreferences)           -- ^ preferences
      -> M.Map PN [LabeledPackageConstraint]  -- ^ global constraints
      -> S.Set PN                             -- ^ global goals
      -> RetryLog Message SolverFailure (Assignment, RevDepMap)
solve sc cinfo pkgConfigDB idx userPrefs userConstraints userGoals =
  explorePhase       $
  detectCycles       $
  trav (
    heuristicsPhase  .
    preferencesPhase .
    validationPhase
  ) $
  validationCata    $
  trav prunePhase   $
  -- trav myCheckPhase $
  buildPhase
  where
    explorePhase     = backjumpAndExplore (maxBackjumps sc)
                                          (enableBackjumping sc)
                                          (fineGrainedConflicts sc)
                                          (countConflicts sc)
                                          idx

    detectCycles     = detectCyclesPhase

    heuristicsPhase  =
      let
          sortGoals = case goalOrder sc of
                        Nothing -> goalChoiceHeuristics .
                                   P.deferSetupExeChoices .
                                   P.deferWeakFlagChoices .
                                   P.preferBaseGoalChoice
                        Just order -> P.firstGoal .
                                      P.sortGoals order
          PruneAfterFirstSuccess prune = pruneAfterFirstSuccess sc
      in sortGoals .
         (if prune then P.pruneAfterFirstSuccess else id)

    preferencesPhase = P.preferLinked .
                       P.preferPackagePreferences userPrefs

    validationPhase  = P.enforcePackageConstraints userConstraints .
                       P.enforceManualFlags userConstraints

    validationCata   = P.enforceSingleInstanceRestriction .
                       validateLinking idx .
                       validateTree cinfo pkgConfigDB idx

    prunePhase =
      (if asBool (avoidReinstalls sc) then P.avoidReinstalls (const True) else id) .
      (case onlyConstrained sc of
        OnlyConstrainedAll ->
          P.onlyConstrained pkgIsExplicit
        OnlyConstrainedNone ->
          id)

    buildPhase = buildTree idx (S.toList userGoals)

    allExplicit = M.keysSet userConstraints `S.union` userGoals

    pkgIsExplicit :: PN -> Bool
    pkgIsExplicit pn = S.member pn allExplicit

    -- When --reorder-goals is set, we use preferReallyEasyGoalChoices, which
    -- prefers (keeps) goals only if the have 0 or 1 enabled choice.
    --
    -- In the past, we furthermore used P.firstGoal to trim down the goal choice nodes
    -- to just a single option. This was a way to work around a space leak that was
    -- unnecessary and is now fixed, so we no longer do it.
    --
    -- If --count-conflicts is active, it will then choose among the remaining goals
    -- the one that has been responsible for the most conflicts so far.
    --
    -- Otherwise, we simply choose the first remaining goal.
    --
    goalChoiceHeuristics
      | asBool (reorderGoals sc) = P.preferReallyEasyGoalChoices
      | otherwise                = id {- P.firstGoal -}

-- myCheckPhase :: TreeTrav () QGoalReason QGoalReason
-- myCheckPhase = go
--   where
--     go (PChoiceF (Q pp pn) revdepmap gr ws) | False
--       = FailF
--         _
--         NotExplicit
--     go x
--       = x

-- | Replace all goal reasons with a dummy goal reason in the tree
--
-- This is useful for debugging (when experimenting with the impact of GRs)
_removeGR :: Tree d c -> Tree d QGoalReason
_removeGR = trav go
  where
   go :: TreeF d c (Tree d QGoalReason) -> TreeF d QGoalReason (Tree d QGoalReason)
   go (PChoiceF qpn rdm _       psq) = PChoiceF qpn rdm dummy       psq
   go (FChoiceF qfn rdm _ a b d psq) = FChoiceF qfn rdm dummy a b d psq
   go (SChoiceF qsn rdm _ a     psq) = SChoiceF qsn rdm dummy a     psq
   go (GoalChoiceF  rdm         psq) = GoalChoiceF  rdm             (goG psq)
   go (DoneF rdm s)                  = DoneF rdm s
   go (FailF cs reason)              = FailF cs reason

   goG :: PSQ (Goal QPN) (Tree d QGoalReason) -> PSQ (Goal QPN) (Tree d QGoalReason)
   goG = PSQ.fromList
       . L.map (\(Goal var _, subtree) -> (Goal var dummy, subtree))
       . PSQ.toList

   dummy :: QGoalReason
   dummy =
       DependencyGoal $
       DependencyReason
           (Q (PackagePath Host QualToplevel) (mkPackageName "$"))
           M.empty S.empty
