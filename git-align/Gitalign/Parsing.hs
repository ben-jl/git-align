{-# LANGUAGE OverloadedStrings #-}
module Gitalign.Parsing (
        shaFromDirectoryParser,
        parseSHA,
        parseCommitLine,
        parseCommits) where

--import Gitalign.Types (Commit(CommitT))
--import Gitalign.Types (Commit(CommitT))
import           Data.Attoparsec.Text (Parser, char, count, endOfLine,
                                       maybeResult, satisfy, string, parse)
import Control.Applicative ((<|>), Alternative (many))
import Data.Char (isAlphaNum)
import Data.Text (Text, pack)
import Prelude (return, (<>), ($), (<$>),(>>))
import Gitalign.Types (Commit(CommitT))
import Data.Maybe (Maybe (Nothing, Just))


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

resolveCommitsFromFind :: [(Text, Text)] -> [Commit]
resolveCommitsFromFind = 
    let resolveCommitsFromFind' acc [] = acc
        resolveCommitsFromFind' acc ((s,raw):ls) = 
            case  maybeResult (parse parseSHA s) of
                Nothing -> resolveCommitsFromFind' acc ls
                Just sha -> case maybeResult (parse parseCommits raw) of
                                Just f -> f sha : resolveCommitsFromFind' acc ls
                                Nothing -> resolveCommitsFromFind' acc ls
    in
        resolveCommitsFromFind' []
