module TFSTypes (TFSConnectionCfg(..)) where

import Data.Text ( Text )

type TFSApiKey = Text 
type TFSUser = Text

data TFSConnectionCfg = TFSConnectionCfg TFSApiKey TFSUser
    deriving (Show, Eq)
