{-# OPTIONS_GHC -Wno-orphans #-}
module UnitTests.Distribution.System
    ( tests
    ) where

import Distribution.Parsec
import Distribution.Pretty
import Distribution.System
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Property, (===))
import Test.QuickCheck.Instances.Cabal ()

textRoundtrip :: (Show a, Eq a, Pretty a, Parsec a) => a -> Property
textRoundtrip x = simpleParsec (prettyShow x) === Just x

tests :: [TestTree]
tests =
    [ testProperty "Text OS round trip"       (textRoundtrip :: OS -> Property)
    , testProperty "Text Arch round trip"     (textRoundtrip :: Arch -> Property)
    , testProperty "Text Platform round trip" (textRoundtrip :: Platform -> Property)
    , testCase "Linux target triples classify known libc environments" $ do
        mapM_
          (\(triple, expected) -> platformFromTriple triple @?= Just expected)
          [ ("x86_64-unknown-linux-gnu", Platform X86_64 Linux)
          , ("x86_64-unknown-linux-musl", Platform X86_64 Linux)
          , ("arm-unknown-linux-gnueabi", Platform Arm Linux)
          , ("arm-unknown-linux-gnueabihf", Platform Arm Linux)
          ]
        platformFromTriple "aarch64-apple-darwin" @?= Just (Platform AArch64 OSX)
        platformFromTriple "wasm32-unknown-wasi" @?= Just (Platform Wasm32 Wasi)
        platformFromTriple "x86_64-unknown-gnu" @?= Just (Platform X86_64 Hurd)
        platformFromTriple "aarch64-unknown-linux-android" @?= Just (Platform AArch64 Android)
        platformFromTriple "aarch64-unknown-linux-androideabi" @?= Just (Platform AArch64 Android)
        platformFromTriple "x86_64-unknown-nto-qnx" @?= Just (Platform X86_64 (OtherOS "nto-qnx"))
        platformFromTriple "x86_64-unknown-linux-private" @?= Just (Platform X86_64 (OtherOS "linux-private"))
    ]
