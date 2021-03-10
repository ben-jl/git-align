{-# LANGUAGE OverloadedStrings #-}
module Gitalign.Parsing where

import Gitalign.Types (Commit(CommitT))

foo :: Commit
foo = CommitT "one" []
