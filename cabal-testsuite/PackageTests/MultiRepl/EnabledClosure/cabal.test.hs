import Test.Cabal.Prelude

main = do
  cabalTest $ do
    skipIfCIAndWindows 11933
    skipUnlessGhcVersion ">= 9.4"
    -- Note: only the last package is interactive.
    -- this test should load pkg-b too.
    res <- cabalWithStdin "v2-repl" ["--enable-multi-repl","pkg-c", "pkg-a"] "Quu.quu"

    assertOutputContains "- host pkg-b-0 (lib)" res
    assertOutputContains "168" res
