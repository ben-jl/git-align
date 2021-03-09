module Gitalign.Types 

where

import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.HashMap.Strict as HM
import Data.Hashable as H
import qualified Data.ByteString as B
import Data.String (IsString(..))

newtype SHA = SHA B.ByteString deriving (Show, Eq)
instance IsString SHA where
    fromString = SHA . fromString

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

data GitObject = CommitObj Commit | TreeObj Tree | BlobObj Blob
instance (RetrievableBySHA GitObject) where 
    sha o = case o of
                CommitObj comm -> sha comm
                TreeObj tree -> sha tree
                BlobObj blob -> sha blob

data DAG = Head | Node Commit [DAG] deriving (Show, Eq)

                            
newtype Repository = RepositoryT { traceBack :: Int -> Commit -> DAG }
fromObjectRepo :: ObjectRepository GitObject -> Repository
fromObjectRepo r = let resolveRepo hm acc n c = 
                        case n of 
                            0 -> acc
                            n -> resolveRepo hm (Node c [acc]) (n-1) $ CommitT "foo" undefined undefined undefined undefined undefined  in
                   RepositoryT $ resolveRepo (unRepo r) Head

newtype ObjectRepository a = ObjectRepositoryT { unRepo :: RetrievableBySHA a => HM.HashMap SHA [a] }

objectRepoFromList :: (RetrievableBySHA a) => [a] -> ObjectRepository a
objectRepoFromList l = 
    let 
        objectRepoFromList' [] acc = acc
        objectRepoFromList' (x:xs) acc 
            = let updatedMap = 
                    case HM.lookup (sha x) acc of
                        Just curr -> HM.adjust (\_ -> x:curr) (sha x) acc
                        Nothing -> HM.insert (sha x) [x] acc 
               in objectRepoFromList' xs updatedMap
    in ObjectRepositoryT $ objectRepoFromList' l HM.empty
