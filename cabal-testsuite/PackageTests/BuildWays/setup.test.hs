import Test.Cabal.Prelude

opts = ["--enable-shared", "--enable-library-vanilla"]

-- See #10418. Static and dynamic artifacts coexist within either target ABI.
main = setupTest $ recordMode DoNotRecord $ withPackageDb $ do
  skipIfNoSharedLibraries
  withDirectory "p" $ setup_install opts
  withDirectory "q" $ setup_install opts
