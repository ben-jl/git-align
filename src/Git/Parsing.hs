{-# LANGUAGE OverloadedStrings #-}
module Git.Parsing where

import qualified Text.Parsec as P
import Data.Text (Text)
import Data.Functor.Identity

type GitParser = P.Parsec Text ()

parseGitString :: GitParser Text
parseGitString = P.string "git" >> P.char ' ' >> pure "git" 
