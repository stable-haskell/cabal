import Test.Cabal.Prelude
main = setupAndCabalTest $ withPackageDb $ do
    setup_install []
    recordMode DoNotRecord $ do
        ghc84 <- isGhcVersion ">= 8.4"
        -- GHC 8.4+ ghc-pkg prints the SPDX id. Older ghc-pkg prints the
        -- legacy tag Cabal wrote into the registration (BSD3, not BSD-3).
        let lic = if ghc84 then "license: BSD-3-Clause\n" else "license: BSD3\n"
        ghcPkg' "field" ["my", "license"] >>= assertOutputContains lic
