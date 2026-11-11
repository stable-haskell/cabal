{-# OPTIONS_GHC -Wno-orphans #-}
module UnitTests.Distribution.Compat.Prelude
    ( tests
    ) where

-- Regression coverage for the NonEmpty-specialised 'foldr1'/'foldl1':
-- they must agree with 'Data.Foldable' on every input. Importing the
-- compatibility Prelude the way library modules do (with an empty
-- 'Prelude' import) also keeps the re-export list honest: anything
-- removed from it by accident breaks this module's compile.
import Distribution.Compat.Prelude
import Prelude ()

import qualified Data.Foldable as Foldable
import qualified Data.List.NonEmpty as NE
import Test.Tasty
import Test.Tasty.QuickCheck

instance Arbitrary a => Arbitrary (NE.NonEmpty a) where
    arbitrary = (NE.:|) <$> arbitrary <*> arbitrary

tests :: [TestTree]
tests =
    [ testProperty "foldr1 matches Data.Foldable.foldr1" prop_foldr1_matches
    , testProperty "foldl1 matches Data.Foldable.foldl1" prop_foldl1_matches
    , testProperty "foldr1 singleton" prop_foldr1_singleton
    , testProperty "foldl1 singleton" prop_foldl1_singleton
    ]

prop_foldr1_matches :: NE.NonEmpty Int -> Property
prop_foldr1_matches ne = foldr1 (-) ne === Foldable.foldr1 (-) ne

prop_foldl1_matches :: NE.NonEmpty Int -> Property
prop_foldl1_matches ne = foldl1 (-) ne === Foldable.foldl1 (-) ne

prop_foldr1_singleton :: Int -> Property
prop_foldr1_singleton x = foldr1 (-) (x NE.:| []) === x

prop_foldl1_singleton :: Int -> Property
prop_foldl1_singleton x = foldl1 (-) (x NE.:| []) === x
