module Gitalign.Types 

where

import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.HashMap.Strict as HM
import Data.Hashable as H
import qualified Data.ByteString as B

newtype SHA = SHA B.ByteString deriving (Show, Eq)

data EnrichedBy a = SHAOnly SHA | FullValue a deriving (Show, Eq)

type ObjectName = Text
type Email = Text
type TimestampUTC = Integer
type OffsetUTC = Integer

data Blob = BlobT SHA (EnrichedBy Text) deriving (Show, Eq)
instance RetrievableBySHA Blob where sha (BlobT x _) = x
instance Hashable Blob where hashWithSalt s (BlobT sh _) = hashWithSalt s sh

data Tree = TreeT SHA ObjectName [EnrichedBy Blob] [EnrichedBy Tree] deriving (Show, Eq)
instance RetrievableBySHA Tree where sha (TreeT x _ _ _) = x
instance Hashable Tree where hashWithSalt s (TreeT sh _ _ _) = hashWithSalt s sh

data Commit = CommitT {
    commitSha :: SHA,
    commitTree :: EnrichedBy Tree,
    commitParents :: [EnrichedBy Commit],
    commitMessage :: Text,
    commitAuthor :: Contributor,
    commitCommitter :: Contributor
} deriving (Show, Eq)
instance RetrievableBySHA Commit where sha = commitSha
instance Hashable Commit where hashWithSalt s = hashWithSalt s . commitSha

data Contributor = ContributerT ObjectName Email TimestampUTC OffsetUTC deriving (Show, Eq)

class RepositoryContext r where
    repoContents :: RetrievableBySHA b => r [b]
    lookupBySHA :: RetrievableBySHA b => r [b] -> SHA -> b

class RetrievableBySHA a where
    sha :: a -> SHA

instance Hashable SHA where
    hashWithSalt s (SHA x) = hashWithSalt s x

unwrap :: SHA -> B.ByteString
unwrap (SHA x) = x

newtype ObjectRepository a = ObjectRepositoryT { contents :: RetrievableBySHA a => HM.HashMap a [a] }

objectRepoFromList :: (Hashable a, Eq a, RetrievableBySHA a) => [a] -> ObjectRepository a
objectRepoFromList l = 
    let objectRepoFromList' :: (Hashable a, Eq a, RetrievableBySHA a) => [a] -> HM.HashMap a [a] -> HM.HashMap a [a]
        objectRepoFromList' [] acc = acc
        objectRepoFromList' (x:xs) acc 
            = let updatedMap = case HM.lookup x acc of
                                Just curr -> HM.adjust (x :) x acc
                                Nothing -> HM.insert x [x] acc in objectRepoFromList' xs updatedMap in
    ObjectRepositoryT $ objectRepoFromList' l HM.empty
