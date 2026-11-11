{-# OPTIONS_GHC -Wno-orphans #-}
module UnitTests.Distribution.Compat.Graph
    ( tests
    , arbitraryGraph
    ) where

import Distribution.Compat.Graph

import qualified Prelude
import Prelude hiding (null)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Control.Monad
import qualified Data.Graph as G
import Data.Array ((!))
import Data.Maybe
import Data.List (sort)
import Data.Proxy (Proxy (..))
import Distribution.Utils.Structured (Structure (Nominal), Structured (..), nominalStructure)

tests :: [TestTree]
tests =
    [ testProperty "arbitrary unbroken" (prop_arbitrary_unbroken :: Graph Int (Node Int ()) -> Bool)
    , testProperty "nodes consistent" (prop_nodes_consistent :: Graph Int (Node Int ()) -> Bool)
    , testProperty "edges consistent" (prop_edges_consistent :: Graph Int (Node Int ()) -> Property)
    , testProperty "closure consistent" (prop_closure_consistent :: Graph Int (Node Int ()) -> Property)
    , testProperty "structure tag" prop_structure_tag
    -- The properties above fix the key to the 'Node' payload index.
    -- Below they are re-run with 'TKey'/'TNode', where the key is a
    -- dedicated type independent of the node payload, plus unit
    -- tests over a hand-built 'Graph TKey TNode'.
    , testProperty "TKey arbitrary unbroken" (prop_arbitrary_unbroken :: Graph TKey TNode -> Bool)
    , testProperty "TKey nodes consistent" (prop_nodes_consistent :: Graph TKey TNode -> Bool)
    , testProperty "TKey edges consistent" (prop_edges_consistent :: Graph TKey TNode -> Property)
    , testProperty "TKey closure consistent" (prop_closure_consistent :: Graph TKey TNode -> Property)
    , testGroup "key parameter" keyParamTests
    ]

-- Our arbitrary instance does not generate broken graphs
prop_arbitrary_unbroken :: Graph k a -> Bool
prop_arbitrary_unbroken g = Prelude.null (broken g)

-- Every node from 'toList' maps to a vertex which
-- is present in the constructed graph, and maps back
-- to a node correctly.
prop_nodes_consistent :: (Eq a, IsNode k a) => Graph k a -> Bool
prop_nodes_consistent g = all p (toList g)
  where
    (_, vtn, ktv) = toGraph g
    p n = case ktv (nodeKey n) of
            Just v  -> vtn v == n
            Nothing -> False

-- A non-broken graph has the 'nodeNeighbors' of each node
-- equal the recorded adjacent edges in the node graph.
prop_edges_consistent :: IsNode k a => Graph k a -> Property
prop_edges_consistent g = Prelude.null (broken g) ==> all p (toList g)
  where
    (gr, vtn, ktv) = toGraph g
    p n = sort (nodeNeighbors n)
       == sort (map (nodeKey . vtn) (gr ! fromJust (ktv (nodeKey n))))

-- Closure is consistent with reachable
prop_closure_consistent :: (Show a, IsNode k a) => Graph k a -> Property
prop_closure_consistent g =
    not (null g) ==>
    forAll (elements (toList g)) $ \n ->
           Set.fromList (map nodeKey (fromJust (closure g [nodeKey n])))
        == Set.fromList (map (nodeKey . vtn) (G.reachable gr (fromJust (ktv (nodeKey n)))))
  where
    (gr, vtn, ktv) = toGraph g

hasNoDups :: Ord a => [a] -> Bool
hasNoDups = loop Set.empty
  where
    loop _ []       = True
    loop s (x:xs) | s' <- Set.insert x s, Set.size s' > Set.size s
                    = loop s' xs
                  | otherwise
                    = False

-- | Produces a graph of size @len@.  We sample with 'suchThat'; if we
-- dropped duplicate entries our size could be smaller.
arbitraryGraph :: (Ord k, Show k, Arbitrary k, Arbitrary a)
               => Int -> Gen (Graph k (Node k a))
arbitraryGraph len = do
    -- Careful! Assume k is much larger than size.
    ks <- vectorOf len arbitrary `suchThat` hasNoDups
    ns <- forM ks $ \k -> do
        a <- arbitrary
        ns <- listOf (elements ks)
        -- Allow duplicates!
        return (N a k ns)
    return (fromDistinctList ns)

instance (Ord k, Show k, Arbitrary k, Arbitrary a)
      => Arbitrary (Graph k (Node k a)) where
    arbitrary = sized $ \n -> do
        len <- choose (0, n)
        arbitraryGraph len

instance Arbitrary TKey where
    arbitrary = TKey <$> arbitrary

-- | Produces an unbroken graph over 'TNode's, mirroring
-- 'arbitraryGraph' but with a key type independent of the node
-- payload.
arbitraryTGraph :: Int -> Gen (Graph TKey TNode)
arbitraryTGraph len = do
    ks <- vectorOf len arbitrary `suchThat` hasNoDups
    nodes <- forM ks $ \k -> do
        ns <- if Prelude.null ks then pure [] else listOf (elements ks)
        pure (TNode k ns)
    pure (fromDistinctList nodes)

instance Arbitrary (Graph TKey TNode) where
    arbitrary = sized $ \n -> do
        len <- choose (0, n)
        arbitraryTGraph len

-- | A small hand-built graph with a dedicated key type:
-- 1 -> {2, 3}, 2 -> {3}, 3 -> {}.
keyGraph :: Graph TKey TNode
keyGraph = fromDistinctList
    [ TNode (TKey 1) [TKey 2, TKey 3]
    , TNode (TKey 2) [TKey 3]
    , TNode (TKey 3) []
    ]

-- | A graph with a dangling reference: 1 -> {2}, with 2 missing.
brokenKeyGraph :: Graph TKey TNode
brokenKeyGraph = fromDistinctList [TNode (TKey 1) [TKey 2]]

keyParamTests :: [TestTree]
keyParamTests =
    [ testCase "empty/null/size" $ do
        null (empty :: Graph TKey TNode) @?= True
        null keyGraph @?= False
        size keyGraph @?= 3
        keys keyGraph @?= [TKey 1, TKey 2, TKey 3]
        keysSet keyGraph @?= Set.fromList [TKey 1, TKey 2, TKey 3]
    , testCase "member/lookup" $ do
        member (TKey 1) keyGraph @?= True
        member (TKey 9) keyGraph @?= False
        lookup (TKey 2) keyGraph @?= Just (TNode (TKey 2) [TKey 3])
        lookup (TKey 9) keyGraph @?= Nothing
    , testCase "insert/deleteKey/deleteLookup" $ do
        let g' = insert (TNode (TKey 9) []) keyGraph
        member (TKey 9) g' @?= True
        size g' @?= 4
        let g'' = deleteKey (TKey 2) g'
        member (TKey 2) g'' @?= False
        size g'' @?= 3
        let (mdeleted, g''') = deleteLookup (TKey 1) g''
        mdeleted @?= Just (TNode (TKey 1) [TKey 2, TKey 3])
        member (TKey 1) g''' @?= False
    , testCase "neighbors/revNeighbors" $ do
        neighbors keyGraph (TKey 1) @?= Just [TNode (TKey 2) [TKey 3], TNode (TKey 3) []]
        neighbors keyGraph (TKey 3) @?= Just []
        neighbors keyGraph (TKey 9) @?= Nothing
        (Set.fromList <$> revNeighbors keyGraph (TKey 3))
            @?= Just (Set.fromList [TNode (TKey 1) [TKey 2, TKey 3], TNode (TKey 2) [TKey 3]])
        revNeighbors keyGraph (TKey 9) @?= Nothing
    , testCase "closure/revClosure" $ do
        (Set.fromList . map nodeKey <$> closure keyGraph [TKey 1])
            @?= Just (Set.fromList [TKey 1, TKey 2, TKey 3])
        (Set.fromList . map nodeKey <$> revClosure keyGraph [TKey 3])
            @?= Just (Set.fromList [TKey 1, TKey 2, TKey 3])
        closure keyGraph [TKey 9] @?= Nothing
    , testCase "topSort/revTopSort/cycles" $ do
        -- 1 has no incoming edges and 3 has no outgoing edges, so
        -- every topological order starts with 1 and ends with 3.
        let sorted = map nodeKey (topSort keyGraph)
        sort sorted @?= [TKey 1, TKey 2, TKey 3]
        head sorted @?= TKey 1
        last sorted @?= TKey 3
        sort (map nodeKey (revTopSort keyGraph)) @?= [TKey 1, TKey 2, TKey 3]
        cycles keyGraph @?= []
    , testCase "broken" $ do
        broken keyGraph @?= []
        broken brokenKeyGraph @?= [(TNode (TKey 1) [TKey 2], TKey 2 NE.:| [])]
    , testCase "union" $ do
        let left = fromDistinctList [TNode (TKey 1) [TKey 2], TNode (TKey 2) []]
            right = fromDistinctList [TNode (TKey 2) [TKey 1], TNode (TKey 3) []]
        lookup (TKey 2) (unionLeft left right) @?= Just (TNode (TKey 2) [])
        lookup (TKey 2) (unionRight left right) @?= Just (TNode (TKey 2) [TKey 1])
        lookup (TKey 3) (unionLeft left right) @?= Just (TNode (TKey 3) [])
    ]

-- Test key/node pair with the smallest possible instances.
newtype TKey = TKey Int
  deriving (Eq, Ord, Show)

data TNode = TNode TKey [TKey]
  deriving (Eq, Show)

instance IsNode TKey TNode where
  nodeKey (TNode k _) = k
  nodeNeighbors (TNode _ ks) = ks

instance Structured TNode where
  structure = nominalStructure

-- | The 'Structured' instance for 'Graph' must stay usable with only
-- 'Structured a' in scope (no 'Typeable k'), so that generically
-- derived plan types keep compiling. This helper only typechecks
-- while the instance head is weak enough.
graphStructureOf :: Structured a => Proxy (Graph k a) -> Structure
graphStructureOf = structure

prop_structure_tag :: Bool
prop_structure_tag =
  case graphStructureOf (Proxy :: Proxy (Graph TKey TNode)) of
    Nominal _ 0 "Graph" [_] -> True
    _ -> False
