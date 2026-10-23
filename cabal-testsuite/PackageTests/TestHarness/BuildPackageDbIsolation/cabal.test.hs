{-# LANGUAGE ScopedTypeVariables #-}

import Test.Cabal.Prelude

import qualified Control.Monad.Catch as MC
import qualified Data.Map as Map
import Distribution.Simple.Compiler (AbiTag (..), Compiler (..), compilerTargetAbi, compilerVersion)
import Distribution.Pretty (prettyShow)
import System.Directory (createDirectoryIfMissing)

main :: IO ()
main = cabalTest $ do
  assertBool "a whitespace-separated Build compiler field must be detected" $
    projectMaySelectSeparateBuildCompiler "with-build-compiler : /other/ghc\n"
  assertBool "an imported project may select a separate Build compiler" $
    projectMaySelectSeparateBuildCompiler "import: cross.project\n"
  assertBool "an imported project suppresses implicit Host DB inheritance" $
    projectHasBuildConfiguration "optional-import : build.project\n"
  assertBool "ordinary project fields do not select a Build compiler" $
    not (projectMaySelectSeparateBuildCompiler "optimization: 2\n")
  let root = "test-root"
  assertEqual "CLI project-file overrides select the overlay's actual project"
    (root </> "cross.project")
    (projectConfigFileFromArgs root "cabal.project" ["--project-file", "cross.project"])
  assertEqual "the final CLI project option wins, excluding executable arguments"
    (root </> "subdir" </> "last.project")
    (projectConfigFileFromArgs root "cabal.project"
      ["--project-file=ignored.project", "--project-dir=subdir", "--project-file=last.project", "--", "--project-file=executable-argument"])

  env <- getTestEnv
  let compiler = testCompiler env
      nativeCompiler = compiler{compilerProperties = Map.delete "Target ABI" (compilerProperties compiler)}
      profilingCompiler = compiler{compilerProperties = Map.insert "Target ABI" "profiling" (compilerProperties compiler)}
      version = prettyShow (compilerVersion compiler)
      nativeLegacy = "ghc-" ++ version
      profilingLegacy = nativeLegacy ++ "-profiling"
  assertBool "native legacy stores must not include profiling ABI namespaces" $
    profilingLegacy `notElem` legacyCompilerStoreDirNames nativeCompiler
  assertBool "profiling legacy stores must not include native ABI namespaces" $
    nativeLegacy `notElem` legacyCompilerStoreDirNames profilingCompiler
  assertEqual "legacy stores retain the selected compiler build ABI"
    [nativeLegacy ++ "-73bf"]
    (legacyCompilerStoreDirNames nativeCompiler{compilerAbiTag = AbiTag "73bf"})
  assertEqual "legacy stores retain build and profiling ABIs independently"
    [nativeLegacy ++ "-73bf-profiling"]
    (legacyCompilerStoreDirNames profilingCompiler{compilerAbiTag = AbiTag "73bf"})

  let oppositeAbi = if compilerTargetAbi compiler == "profiling" then "" else "-profiling"
      oppositeStore = testSystemTmpDir env </> "opposite-abi-store"
      oppositePackage = oppositeStore </> (nativeLegacy ++ oppositeAbi) </> "lib" </> "opposite-1"
  liftIO $ createDirectoryIfMissing True oppositePackage
  liftIO $ writeFile (oppositePackage </> "cabal-hash.txt") "documentation: True\n"
  result <- MC.try $ withStoreDir oppositeStore (findDependencyInStore "opposite")
  case result of
    Left (_ :: IOError) -> pure ()
    Right selected -> assertFailure $ "selected an opposite-ABI legacy store entry: " ++ selected
