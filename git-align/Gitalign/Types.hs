{-# LANGUAGE OverloadedStrings #-}
module Gitalign.Types 

where

import qualified Data.Graph.Types as G
import qualified Data.Graph.DGraph as D
import qualified Data.Graph.Visualize as V
import qualified Data.Hashable as H
import Data.Text ( Text ) 
import Data.String (IsString(fromString))
import Control.Monad (ap)
import qualified Data.GraphViz.Types as GV 
import qualified Data.Graph.Connectivity as GC
                
data Commit = CommitT { commitSHA :: Text, commitParents :: [Commit] } deriving Show
instance H.Hashable Commit where
    hashWithSalt s = H.hashWithSalt s . commitSHA

instance Ord Commit where
    compare c1 c2 = compare (commitSHA c1) (commitSHA c2)

instance Eq Commit where
    (==) c1 c2 = commitSHA c1 == commitSHA c2

instance IsString Commit where
    fromString s = CommitT (fromString s) [] 

newtype Repository = RepositoryT { unRepo :: D.DGraph Commit () } deriving Show

prettyPrint :: Repository -> Text
prettyPrint r = fromString $ D.prettyPrint (unRepo r)

plotRepo :: Repository -> FilePath -> IO FilePath 
plotRepo r =  V.plotDGraphPng (unRepo r)

instance GV.PrintDot Commit where
    toDot = GV.toDot . commitSHA
    unqtDot = GV.toDot

hasParent :: Repository -> Commit -> Commit -> Bool
hasParent r = GC.areConnected (unRepo r)

numChildren :: Repository -> Commit -> Int
numChildren r = D.vertexIndegree (unRepo r)

numParents :: Repository -> Commit -> Int
numParents r = D.vertexOutdegree (unRepo r)

fromCommitList :: [Commit] -> Repository
fromCommitList cts = 
    let fromCommitList' [] acc = RepositoryT acc
        fromCommitList' (c:cs) acc = fromCommitList' cs (G.insertEdgePairs ((c,) <$> commitParents c) acc)
    in fromCommitList' cts G.empty
