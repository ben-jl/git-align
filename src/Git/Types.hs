module Git.Types (
        GitObjectType(..), 
        GitTreeObjectElement(..), 
        GitTreeObject(..),
        GitCommitObject(..)
    ) where

import Data.Text (Text)
import Data.Word (Word16)
import qualified Data.HashMap.Strict as HM

type SHA = Text

newtype RawGitRepository = RawGitRepositoryT { internalMap :: HM.HashMap Text GitObject }

data GitObject = GitCommitObject | GitTreeObject | GitBlobObject

data GitObjectType = Blob | Tree deriving (Show,Eq)

data GitBlobObject = GitBlobObj { gitBlobSha256 :: Text, gitBlobContents :: [Word16] } deriving (Show, Eq)

data GitTreeObjectElement = GitTreeObjectElement {
    gitTreeObjElementMode :: [Char],
    gitTreeObjElementType :: GitObjectType,
    gitTreeObjElementSha256 :: Text,
    gitTreeObjElementName :: Text
} deriving (Show, Eq)

data GitTreeObject = GitTreeObjectT {
    gitTreeObjSha256 :: Text,
    gitTreeObjElements :: [GitTreeObjectElement]
} deriving (Show, Eq)

data GitCommitObject = GitCommitObjectT {
    gitCommitObjSha256 :: Text,
    gitCommitTreeObjSha256 :: Text,
    gitCommitObjAuthor :: Text,
    gitCommitObjCommitor :: Text,
    gitCommitObjMessage :: Text
} deriving (Show, Eq)
