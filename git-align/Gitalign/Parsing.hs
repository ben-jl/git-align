{-# LANGUAGE OverloadedStrings #-}
module Gitalign.Parsing (
        shaFromDirectoryParser,
        parseSHA,
        parseCommitLine,
        parseCommits) where

--import Gitalign.Types (Commit(CommitT))
--import Gitalign.Types (Commit(CommitT))
import Data.Attoparsec.Text (Parser, count, char, satisfy, string, endOfLine)
import Control.Applicative ((<|>), Alternative (many))
import Data.Char (isAlphaNum)
import Data.Text (Text, pack)
import Prelude (return, (<>), ($), (<$>),(>>))
import Gitalign.Types (Commit(CommitT))


shaFromDirectoryParser :: Parser Text
shaFromDirectoryParser = do
    firstTwo <- count 2 (satisfy isAlphaNum)
    _ <- char '/' <|> char '\\'
    remainder <- count 38 (satisfy isAlphaNum)
    return $ pack (firstTwo <> remainder)

parseSHA :: Parser Text
parseSHA = pack <$> count 40 (satisfy isAlphaNum)

parseCommitLine :: Parser Text
parseCommitLine = do
    _ <- string "commit" >> string " "
    sha <- parseSHA
    _ <- endOfLine 
    return sha

parseCommits :: Parser (Text -> Commit)
parseCommits = do
    shas <- many parseCommitLine
    _ <- string "tree" >> string " " >> parseSHA 
    _ <- endOfLine 
    let parentCommits = [CommitT c [] | c <- shas]
    return $ \x ->  CommitT x parentCommits
