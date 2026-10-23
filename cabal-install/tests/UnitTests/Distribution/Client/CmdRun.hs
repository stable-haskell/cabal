module UnitTests.Distribution.Client.CmdRun (tests) where

import qualified Data.Map as Map
import Distribution.Client.CmdRun (collectRuntimeLibraryDirs, sameStageDependencies)
import Distribution.Client.ProjectPlanning.Stage (WithStage (..))
import Distribution.Solver.Types.Stage (Stage (..))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)

tests :: [TestTree]
tests =
  [ testCase "runtime library dependency closure" $ do
      let hostExe = WithStage Host "exe" :: WithStage String
          hostA = WithStage Host "A"
          hostB = WithStage Host "B"
          buildTool = WithStage Build "build-tool"
          libraryGraph =
            Map.fromList
              [ (hostExe, (["exe-dynlib"], [hostA, buildTool]))
              , (hostA, (["A-dynlib", "shared-dynlib"], [hostB, hostB, buildTool]))
              , (hostB, (["B-dynlib", "shared-dynlib"], []))
              , (buildTool, (["build-dynlib"], []))
              ]
          lookupLibrary key@(WithStage stage _) = do
            (dirs, dependencies) <- Map.lookup key libraryGraph
            return (dirs, sameStageDependencies stage dependencies)
      assertEqual
        "Include the selected package and transitive Host libraries once, excluding Build libraries at every edge"
        ["exe-dynlib", "A-dynlib", "shared-dynlib", "B-dynlib"]
        (collectRuntimeLibraryDirs lookupLibrary [hostExe])
  ]
