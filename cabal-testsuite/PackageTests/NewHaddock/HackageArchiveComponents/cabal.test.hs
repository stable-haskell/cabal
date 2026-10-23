import Test.Cabal.Prelude

main = cabalTest $ recordMode DoNotRecord $ do
  cabal "haddock" ["--haddock-for-hackage", "--haddock-all", "-j2"]
  env <- getTestEnv
  let archive = testDistDir env </> "archive-components-0.1-docs.tar.gz"
  shouldExist archive
  contents <- tar' ["-tzf", archive]
  forM_ [ "Public.html"
        , "archive-components.haddock"
        , "internal/Internal.html"
        , "internal/internal.haddock"
        ] $ \member ->
    assertOutputContains ("archive-components-0.1-docs/" ++ member) contents
