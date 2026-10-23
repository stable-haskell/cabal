import Distribution.Simple.Compiler (compilerTargetName, renderTargetName)
import System.FilePath ( joinPath )

import Test.Cabal.Prelude

-- https://github.com/haskell/cabal/issues/7679
-- https://github.com/haskell/cabal/issues/8400

main = cabalTest . void $ do
    env <- getTestEnv
    target <- either fail (pure . renderTargetName) (compilerTargetName (testPlatform env) (testCompiler env))
    res <- cabal' "list-bin" ["exe:testexe"]

    let path = joinPath ["store", "host", target, "bin", "testexe"]
    assertOutputContains path res
