import Test.Cabal.Prelude
-- Test building a profiled library/executable which uses Template Haskell
-- (Setup/build tools remain ordinary; target evaluation uses profiling)
main = setupAndCabalTest $ do
    skipUnless "no profiling libs" =<< hasProfiledLibraries
    setup_build ["--enable-library-profiling",
                 "--enable-profiling"]
