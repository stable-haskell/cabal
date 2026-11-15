module UnitTests.Distribution.Client.CmdRun (tests) where

import qualified Data.Map as Map
import Distribution.Client.CmdRun (collectRuntimeLibraryDirs, sameMachineDependencies)
import Distribution.Client.ProjectPlanning.Machine (WithMachine (..))
import Distribution.Solver.Types.Machine (Machine (..))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)

tests :: [TestTree]
tests =
  [ testCase "runtime library dependency closure" $ do
      let hostExe = WithMachine Host "exe" :: WithMachine String
          hostA = WithMachine Host "A"
          hostB = WithMachine Host "B"
          buildTool = WithMachine Build "build-tool"
          libraryGraph =
            Map.fromList
              [ (hostExe, (["exe-dynlib"], [hostA, buildTool]))
              , (hostA, (["A-dynlib", "shared-dynlib"], [hostB, hostB, buildTool]))
              , (hostB, (["B-dynlib", "shared-dynlib"], []))
              , (buildTool, (["build-dynlib"], []))
              ]
          lookupLibrary key@(WithMachine machine _) = do
            (dirs, dependencies) <- Map.lookup key libraryGraph
            return (dirs, sameMachineDependencies machine dependencies)
      assertEqual
        "Include the selected package and transitive Host libraries once, excluding Build libraries at every edge"
        ["exe-dynlib", "A-dynlib", "shared-dynlib", "B-dynlib"]
        (collectRuntimeLibraryDirs lookupLibrary [hostExe])
  ]
