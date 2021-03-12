{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Gitalign.Types 
    (
        Repository(..)
        , SHA(..)
        , numParents
        , numChildren
        , isChildOf
        , count
        , repoFromList
    )
where

import Prelude (Bool, Eq ((/=), (==)), Int,
                Ord (compare, (<=)), Show, String, length, map,
                maximum, ($), (.), (<$>), (<*>), otherwise, undefined, Functor (fmap), return, not)
import Data.Char
import Data.Graph.Types qualified as  G 
import Data.Graph.DGraph qualified as D
import Data.Hashable qualified as H
import Data.HashMap.Strict qualified as HM
import Data.Text ( Text, pack ) 
import qualified Gitalign.Internal.Types  as T (isConnected)
import Test.QuickCheck

newtype SHA = SHA Text deriving (Show, H.Hashable, Eq)
newtype Repository = R {unRepo :: D.DGraph Int ()} deriving Show

numChildren :: Repository -> SHA -> Int
numChildren r s = D.vertexIndegree (unRepo r) (H.hash s)

numParents :: Repository -> SHA -> Int
numParents r s = D.vertexOutdegree  (unRepo r)  (H.hash s)

count :: Repository -> Int
count = G.order . unRepo

isChildOf :: Repository -> SHA -> SHA -> Bool
isChildOf r x y = T.isConnected () (unRepo r) (H.hash x) (H.hash y)


repoFromList :: [(SHA, [SHA])] -> Repository
repoFromList ls = 
    let converted = map (\(x, xs) -> (H.hash x, map (\y -> (H.hash y, ())) xs)) ls in
    R $ G.fromList converted

instance Arbitrary Repository where
    arbitrary = do
        g <- arbitrary :: Gen (D.DGraph Int ())
        return $ R g

instance Arbitrary SHA where
    arbitrary = do
        s <- arbitrary `suchThat` (not . (==) []) :: Gen String
        return $ SHA (pack s)
