import Test.Cabal.Prelude
main = setupAndCabalTest $ withPackageDb $ do
    setup_install []
    recordMode DoNotRecord $ do
        ghc84 <- isGhcVersion ">= 8.4"
        -- Cabal registers SPDX identifiers for all compiler versions. Old
        -- ghc-pkg prints this license field as "BSD-3".
        let lic = if ghc84 then "license: BSD-3-Clause\n" else "license: BSD-3\n"
        ghcPkg' "field" ["my", "license"] >>= assertOutputContains lic
