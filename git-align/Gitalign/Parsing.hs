{-# LANGUAGE OverloadedStrings #-}
module Gitalign.Parsing (
        shaFromDirectoryParser
    ) where

--import Gitalign.Types (Commit(CommitT))
import Data.Attoparsec.Text (Parser, count, char, satisfy)
import Control.Applicative ((<|>))
import Data.Char (isAlphaNum)
import Data.Text (Text, pack)
import Prelude (return, (<>), ($))

shaFromDirectoryParser :: Parser Text
shaFromDirectoryParser = do
    firstTwo <- count 2 (satisfy isAlphaNum)
    _ <- char '/' <|> char '\\'
    remainder <- count 38 (satisfy isAlphaNum)
    return $ pack (firstTwo <> remainder)
