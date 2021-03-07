module GitTypes where

import Data.Text (Text)

type GitApiKey = Text
type GitUser = Text

data GitConnectionCfg = GitConnectionCfg GitApiKey GitUser
