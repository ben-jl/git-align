module Git.Types where

import Data.Text (Text)
import Data.Word (Word16)

type GitApiKey = Text
type GitUser = Text

data GitConnectionCfg = GitConnectionCfg GitApiKey GitUser deriving (Show, Eq)

data GitRepository = GitRepository {
    gitRepoLocation :: Text,
    gitRepoName :: Text
} deriving (Show, Eq)

data GitBlob = GitBlob { gitBlobSha256 :: Text, gitBlobContents :: [Word16] } deriving (Show, Eq)

data GitTreeObjectElement = GitTreeObjectElement {
    gitTreeObjElementMode :: Int,
    gitTreeObjElementType :: Text,
    gitTreeObjElementSha256 :: Text,
    gitTreeObjElementName :: Text
} deriving (Show, Eq)

data GitTreeObject = GitTreeObject {
    gitTreeObjSha256 :: Text,
    gitTreeObjElements :: [GitTreeObjectElement]
} deriving (Show, Eq)

data GitCommitObject = GitCommitObject {
    gitCommitObjSha256 :: Text,
    gitCommitTreeObjSha256 :: Text,
    gitCommitObjAuthor :: Text,
    gitCommitObjCommitor :: Text,
    gitCommitObjMessage :: Text
} deriving (Show, Eq)

data GitCommand = 
    GitStatus 
    | GitClone GitConnectionCfg
    | GitPull
    | GitCommit Text
    deriving (Show, Eq)
