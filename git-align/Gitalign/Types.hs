{-# LANGUAGE OverloadedStrings #-}
module Gitalign.Types 

where

import qualified Data.Graph.Types as G
import qualified Data.Graph.DGraph as D
import qualified Data.Hashable as H
import Data.Text ( Text ) 
import Data.String (IsString(fromString))
                
data Commit = CommitT { commitSHA :: Text, commitParents :: [Commit] } deriving Show
instance H.Hashable Commit where
    hashWithSalt s = H.hashWithSalt s . commitSHA

instance Eq Commit where
    (==) c1 c2 = commitSHA c1 == commitSHA c2

instance IsString Commit where
    fromString s = CommitT (fromString s) [] 

newtype Repository = RepositoryT { unRepo :: D.DGraph Commit () } deriving Show

parents :: Commit -> Repository -> [Commit]
parents c r = G.reachableAdjacentVertices (unRepo r) c

fromCommitList :: [Commit] -> Repository
fromCommitList cts = 
    let fromCommitList' [] acc = RepositoryT acc
        fromCommitList' (c:cs) acc = fromCommitList' cs (G.insertEdgePairs ((c,) <$> commitParents c) acc)
    in fromCommitList' cts G.empty
