module TFS.Types (TFSConnectionCfg(..)) where

import Data.Text ( Text )

type TFSApiKey = Text 
type TFSUser = Text

data TFSConnectionCfg = TFSConnectionCfg TFSApiKey TFSUser
    deriving (Show, Eq)

data TFSRequestParameters =
    TFSChangesetRequest 
    | TFSBranchRequest { branchReqPath :: Text, branchReqncludeParent :: Maybe Bool, branchReqIncludeChildren :: Maybe Bool}
    deriving (Show, Eq)

data TFSConnectionContext = TFSConnectionContext {
    connectionConfig :: TFSConnectionCfg,
    organization :: Text,
    project :: Text,
    apiVersion :: Text,
    requestParams :: TFSRequestParameters
} deriving (Show, Eq)

data TFSIdentity = TFSIdentity {
    descriptor :: Text,
    displayName :: Text,
    tfsId :: Text,
    isDeletedInOrigin :: Bool,
    tfsIdUrl :: Text
} deriving (Show, Eq)

data TFSChangeset = TFSChangeset {
    tfsAuthor :: TFSIdentity,
    tfsChangesetId :: Int,
    tfsCheckedInBy :: TFSIdentity,
    tfsComment :: Text,
    tfsCommentTruncated :: Bool,
    tfsCreatedDate :: Text,
    tfsChangesetUrl :: Text
} deriving (Show, Eq)

data TFSBranchMapping = TFSBranchMapping { branchDepth :: Text, branchServerItem :: Text, branchType :: Text } 
    deriving (Show, Eq)

newtype TFSShallowBranch = TFSShallowBranch { shallowBranchPath :: Text} 
    deriving (Show, Eq)

data TFSBranch = TFSBranch {
    branchChildren :: [TFSBranch],
    branchCreatedDate :: Text,
    branchDescription :: Text,
    branchIsDeleted :: Bool,
    branchMappings :: [TFSBranchMapping],
    branchOwner :: TFSIdentity,
    branchPath :: Text,
    branchRelatedBranches :: [TFSShallowBranch],
    branchUrl :: Text
} deriving (Show, Eq)
