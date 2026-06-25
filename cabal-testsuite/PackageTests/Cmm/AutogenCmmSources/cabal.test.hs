import Test.Cabal.Prelude

-- The 'demo' executable calls a foreign prim ('aToMyWordzh') that is only
-- defined in the C-- source generated into the build directory and declared
-- via 'autogen-cmm-sources'. A successful run proves the generated file was
-- compiled and linked.
main = cabalTest $ do
    skipUnlessGhcVersion ">= 7.8"
    res <- cabal' "v2-run" ["demo"]
    assertOutputContains "In Box we have 0x" res
