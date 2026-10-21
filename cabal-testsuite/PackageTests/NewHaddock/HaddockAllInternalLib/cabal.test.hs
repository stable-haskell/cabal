import Distribution.Simple.Compiler (compilerTargetName, renderTargetName)
import Test.Cabal.Prelude
-- Test that `cabal haddock --haddock-all` correctly finds the .haddock
-- interface file for an internal sub-library (no "could not find link
-- destinations" warning).
main = cabalTest $ do
    res <- cabal' "haddock" ["--haddock-all"]
    assertOutputDoesNotContain "Could not find documentation for exported module" res
    env <- getTestEnv
    target <- either fail pure (compilerTargetName (testPlatform env) (testCompiler env))
    let storeDir = testDistDir env </> "store"
        packageDb = storeDir </> "host" </> renderTargetName target </> "package.conf.d"
    interfaceFiles <- assertGlobMatches storeDir "**/share/doc/html/internal/internal.haddock"
    interfaceFile <- case interfaceFiles of
        [path] -> pure path
        _ -> fail "expected exactly one installed internal-library Haddock interface"
    registered <- runProgramM ghcPkgProgram
      ["--package-db=" ++ packageDb, "field", "--ipid", "haddock-internal-0.1.0.0-internal", "haddock-interfaces"] Nothing
    assertOutputContains interfaceFile registered
