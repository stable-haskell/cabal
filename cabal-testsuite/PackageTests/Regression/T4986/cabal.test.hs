import Test.Cabal.Prelude
main = cabalTest $ do
    cabal "v2-configure" []
    env <- getTestEnv
    let localConfig = testCurrentDir env </> "cabal.project.local"
    shouldExist localConfig
    recordMode DoNotRecord $ do
        cabal "v2-configure" ["--enable-tests"]
        assertFileDoesContain localConfig "tests: True"
