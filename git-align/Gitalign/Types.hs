module Gitalign.Types 

where

import Data.Text (Text)
import qualified Data.HashMap.Strict as HM

newtype ObjectRepository = ObjectRepositoryT { unObjectRepository ::  HM.HashMap SHA Commit }

newtype SHA = SHA Text
type ObjectName = Text
type Email = Text
type TimestampUTC = Integer
type OffsetUTC = Integer


data Blob = BlobT SHA Text
data Tree = TreeT SHA ObjectName [Blob] [Tree]
data Commit = CommitT {
    commitSha :: SHA,
    commitTree :: Tree,
    commitParents :: [Commit],
    commitMessage :: Text,
    commitAuthor :: Contributor,
    commitCommitter :: Contributor
}


data Contributor = ContributerT ObjectName Email TimestampUTC OffsetUTC


