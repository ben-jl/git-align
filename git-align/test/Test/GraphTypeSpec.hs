module Test.GraphTypeSpec where

import Prelude
import Test.QuickCheck ( (===), (==>) )
import Gitalign
import Data.HashMap.Strict qualified as HM
import Data.List qualified as L
import Test.Hspec.QuickCheck ( prop )
import Test.Hspec ( hspec, Spec )

graphTypeSpec :: IO ()
graphTypeSpec = hspec $ do
    prop_isDirectAncestorAfterInsertLinkInsertLink
    prop_graphFromListRespectsLength
    prop_insertElementIsIdempotent
    prop_hasMemberAfterInsertIsTrue
    prop_graphHasElementChildAfterInsertLink
    prop_graphHasElementParentAfterInsertLink

prop_graphFromListRespectsLength ::  Spec
prop_graphFromListRespectsLength =
    prop "repo created has same length as distinct elements" $ do
        \xs -> let trimmed = L.nubBy (\a b -> fst a == fst b) (xs :: [(GraphElement Int String, [GraphElement Int String])]) in
            length (HM.keys (maybe HM.empty unHashMap (graphFromList trimmed))) === length trimmed

prop_insertElementIsIdempotent :: Spec
prop_insertElementIsIdempotent=
    prop "Order of digraph is unchanged when insert is called twice" $ do
        \g e -> let insertedOnce = insertElement (g :: Digraph Int String) e in
                HM.size (unHashMap insertedOnce) == HM.size (unHashMap (insertElement insertedOnce e))

prop_hasMemberAfterInsertIsTrue :: Spec
prop_hasMemberAfterInsertIsTrue =
    prop "hasMember is always true after insert" $ do
        \g e -> hasMember (insertElement (g :: Digraph Int String) e) e

prop_adjacentAfterInsertLinkAlwaysTrue :: Spec
prop_adjacentAfterInsertLinkAlwaysTrue =
    prop "adjacent after insertLink always true" $ do
        \child parent g -> adjacent (insertLink (g :: Digraph Int String) child parent) child parent

prop_graphHasElementChildAfterInsertLink :: Spec
prop_graphHasElementChildAfterInsertLink =
    prop "graph always contains a child element after insterting link" $ do
        \child parent g -> hasMember (insertLink (g :: Digraph Int String) child parent) child

prop_graphHasElementParentAfterInsertLink :: Spec
prop_graphHasElementParentAfterInsertLink =
    prop "graph always contains a parent element after insterting link" $ do
        \child parent g -> hasMember (insertLink (g :: Digraph Int String) child parent) parent

prop_isDirectAncestorAfterInsertLinkInsertLink :: Spec
prop_isDirectAncestorAfterInsertLinkInsertLink =
    prop "isDirectAncestor after two insert links always true" $ do
        \c1 i1 a1 g ->
            c1 /= i1 && i1 /= a1 && a1 /= c1 ==> isDirectAncestor (insertLink (insertLink (g :: Digraph Int String) c1 i1) i1 a1) c1 a1
