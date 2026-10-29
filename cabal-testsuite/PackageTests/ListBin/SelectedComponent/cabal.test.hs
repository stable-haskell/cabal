import Distribution.Pretty (prettyShow)
import Distribution.Simple.Compiler (compilerId)

import Test.Cabal.Prelude

-- https://github.com/haskell/cabal/issues/7679
-- https://github.com/haskell/cabal/issues/8400
-- list-bin reports the dist build path on this base. Executables are not
-- copied to store/<machine>/<target>/bin.
main = cabalTest . void $ do
    env <- getTestEnv
    res <- cabal' "list-bin" ["exe:testexe"]

    let path =
          "dist"
            </> "build"
            </> prettyShow (testPlatform env)
            </> prettyShow (compilerId (testCompiler env))
            </> "SelectedComponent-1.0.0"
            </> "build"
            </> "testexe"
            </> "testexe"
    assertOutputContains path res
