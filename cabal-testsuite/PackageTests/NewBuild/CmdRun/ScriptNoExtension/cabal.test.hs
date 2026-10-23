import Test.Cabal.Prelude

main = cabalTest . void $ do
    cabal' "v2-run" ["with sp"] >>= assertOutputContains "Hello World"
    cabal' "v2-run" ["with output"] >>= assertOutputContains "Explicit output"
    -- GHC adds .exe on Windows only when the explicit output has no extension.
    cabal' "v2-run" ["without extension"] >>= assertOutputContains "Extensionless output"
