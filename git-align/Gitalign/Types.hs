{-# LANGUAGE OverloadedStrings #-}
module Gitalign.Types 
    (
        Repository(..)
        , Commit(..)
        , fromCommitList
        , numParents
        , numChildren
        , hasParent
        , commitCount
        , peekLatestCommit
        , isDirectAncestor
    )
where

import Prelude (Bool, Eq ((/=), (==)), Int,
                Ord (compare, (<=)), Show, String, length, map,
                maximum, ($), (.), (<$>), (<*>), otherwise)
import Data.Graph.Types qualified as  G 
import Data.Graph.DGraph qualified as D
import Data.Hashable qualified as H
import Data.Text ( Text, pack ) 
import Data.Graph.Connectivity qualified as GC
import Test.QuickCheck qualified as Q
import Data.List qualified
import Data.Maybe ( Maybe(..), fromMaybe )
import Control.Monad ( Monad(return), Functor(fmap), msum )

newtype Repository = RepositoryT { unRepo :: D.DGraph Commit () } deriving stock (Show)
data Commit = CommitT { commitSHA :: !Text, commitParents :: [Commit] } deriving stock (Show)

instance H.Hashable Commit where
    hashWithSalt s = H.hashWithSalt s . commitSHA

instance Ord Commit where
    compare c1 c2 = compare (commitSHA c1) (commitSHA c2)

instance Eq Commit where
    (==) c1 c2 = commitSHA c1 == commitSHA c2

isDirectAncestor :: Commit -> Commit -> Maybe [Commit]
isDirectAncestor c1 c2 =
    let isDirectAncestor' ci1@(CommitT s1 []    ) (CommitT s2 _) Nothing = if s1 == s2 then Just [ci1] else Nothing
        isDirectAncestor' ci1@(CommitT s1 []    ) (CommitT s2 _) (Just g)  = if s1 == s2 then Just (ci1:g) else Nothing
        isDirectAncestor' ci1@(CommitT s1 ls1   ) ci2@(CommitT s2 _) acc 
            | s1 == s2 = Just $  ci1 : fromMaybe [] acc
            | otherwise = 
                let sPath = ci1 : fromMaybe [] acc
                in msum [isDirectAncestor' p ci2 (Just sPath) | p <- ls1]                      
    in
        isDirectAncestor' c1 c2 Nothing 

hasParent :: Repository -> Commit -> Commit -> Bool
hasParent r = GC.areConnected (unRepo r)

numChildren :: Repository -> Commit -> Int
numChildren r = D.vertexIndegree (unRepo r)

numParents :: Repository -> Commit -> Int
numParents r = D.vertexOutdegree (unRepo r)

commitCount :: Repository -> Int
commitCount = G.order . unRepo

peekLatestCommit :: Repository -> Maybe Commit
peekLatestCommit r = case G.vertices (unRepo r) of
    [] -> Nothing
    cs -> Just (maximum cs)

fromCommitList :: [Commit] -> Repository
fromCommitList cts = 
    let fromCommitList' [] acc = RepositoryT acc
        fromCommitList' (c:cs) acc = fromCommitList' cs (case commitParents c of
            [] -> G.insertVertex c acc
            cps -> G.insertEdgePairs ((c,) <$> cps) acc)
    in fromCommitList' cts G.empty


instance Q.Arbitrary Repository where
    arbitrary = do
        strList <- Q.listOf (Q.arbitrary `Q.suchThat` (\c -> length c <= 8) :: Q.Gen String) 
        let subseqs = Data.List.take (length strList) (Data.List.permutations strList)
        let commits = Data.List.zipWith (\x y -> CommitT x (CommitT <$> y <*> [])) (pack <$> strList) (fmap (fmap pack) subseqs)
        return $ fromCommitList commits

instance Q.Arbitrary Commit where
    arbitrary = do
        textList <- Q.arbitrary :: Q.Gen [String]
        currSha <- Q.arbitrary `Q.suchThat` (/=) [] :: Q.Gen String
        return $ CommitT (pack currSha) (CommitT <$> map pack textList <*> [])
