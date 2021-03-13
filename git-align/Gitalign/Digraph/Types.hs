module Gitalign.Digraph.Types 
    (
    Digraph(..),hasMember
    , graphFromList,
    GraphElement,
    insertElement
    ,insertLink
    ,adjacent
    ,isDirectAncestor)
where
import Data.Hashable as H
import Prelude
import Data.HashMap.Strict as HM
import Data.HashSet as HS
import Data.Maybe (listToMaybe, fromMaybe)
import Test.QuickCheck
import Data.List

import Control.Monad


data GraphElement v a = GE { key::  v, attr :: a} 

instance (Show v, Show a) => Show (GraphElement v a) where
    show (GE v a) = "GE " ++ show v ++ " " ++ show a

instance H.Hashable v => H.Hashable (GraphElement v a) where
    hashWithSalt i g = H.hashWithSalt i (key g)

instance (H.Hashable v, Eq v, Eq a) => Eq (GraphElement v a) where
    g == h = (key g == key h) && (attr g == attr h)

newtype Digraph v a = DG { unHashMap :: HM.HashMap (GraphElement v a) (HS.HashSet (GraphElement v a))}
    deriving Show

-- Create a Digraph from an adjacency list of graph elements
graphFromList ::(H.Hashable v, Eq v, Eq a) => [(GraphElement v a, [GraphElement v a])] -> Maybe (Digraph v a)
graphFromList gs = Just (DG $ Data.List.foldl' (\b (a1, as) -> HM.union b (HM.singleton a1 (mconcat [HS.singleton a | a <- as]))) HM.empty gs)

-- Predicate to determine if a given element is a member of a digraph
hasMember :: (Hashable v, Eq v, Eq a) => Digraph v a -> GraphElement v a -> Bool
hasMember g e = HM.member e (unHashMap g)

-- Inserts a graph element into a digraph, leaving the old value alone if present
insertElement :: (Hashable v, Eq v, Eq a) => Digraph v a -> GraphElement v a -> Digraph v a
insertElement g e = DG (HM.union (unHashMap g) (HM.singleton e HS.empty) )

-- Create directed link between two elements, inserting one or the other if necessary, otherwise leave graph untouched
insertLink :: (Hashable v, Eq v, Eq a) => Digraph v a -> GraphElement v a -> GraphElement v a -> Digraph v a
insertLink g@(DG h) child parent 
    | hasMember g child = DG (HM.adjust (\x -> x <> HS.singleton parent) child (HM.union h (HM.singleton parent HS.empty)))
    | otherwise = insertLink (insertElement g child) child parent

-- Return true if two graph elements are adjacent
adjacent :: (Hashable v, Eq v, Eq a) => Digraph v a -> GraphElement v a -> GraphElement v a -> Bool
adjacent g@(DG h) child parent = hasMember g child && HS.member parent (h ! child)

-- Return true if one graph element is direct ancestor of child (e.g. no branch)
-- TODO prop test
isDirectAncestor :: (Hashable v, Eq v, Eq a) => Digraph v a -> GraphElement v a -> GraphElement v a -> Bool
isDirectAncestor g@(DG h) descendent ancestor 
    | not (hasMember g descendent) || not (hasMember g ancestor) = False
    | adjacent g descendent ancestor = True
    | otherwise = case HS.toList (h ! descendent) of
        [] -> False
        cps -> and [isDirectAncestor g cp ancestor | cp <- cps]


instance (Arbitrary v, Arbitrary a, H.Hashable v) => Arbitrary (GraphElement v a) where
    arbitrary = GE <$> (arbitrary :: Gen v) <*> (arbitrary :: Gen a)

instance (Arbitrary  v, Arbitrary a, H.Hashable v, Eq v, Eq a) => Arbitrary (Digraph v a) where
    arbitrary = do
        n <- choose (0,5) :: Gen Int
        bases <- vectorOf n (arbitrary :: Gen (GraphElement v a))
        let nubbed = nubBy (\l r -> key l == key r) bases
        let tuples = [(p,ps) | p <- nubbed, ps <- take 2 (Data.List.subsequences nubbed)]
        return (fromMaybe (DG HM.empty) (graphFromList tuples))
