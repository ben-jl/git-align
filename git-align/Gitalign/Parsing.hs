{-# LANGUAGE OverloadedStrings #-}
module Gitalign.Parsing (
        ) where

import           Data.Attoparsec.Text (Parser, char, count, endOfLine, satisfy, string)
import Control.Applicative ((<|>), Alternative (many))
import Data.Char (isAlphaNum)
import Data.Text (Text, pack)
import Prelude (return, (<>), ($), (<$>),(>>))
import Gitalign.Types (SHA)

-- shaFromDirectoryParser :: Parser SHA
-- shaFromDirectoryParser = do
--     firstTwo <- count 2 (satisfy isAlphaNum)
--     _ <- char '/' <|> char '\\'
--     remainder <- count 38 (satisfy isAlphaNum)
--     return $ SHA (pack (firstTwo <> remainder))

-- parseSHA :: Parser SHA
-- parseSHA = SHA $ pack <$> count 40 (satisfy isAlphaNum)

-- parseCommitLine :: Parser SHA
-- parseCommitLine = do
--     _ <- string "commit" >> string " "
--     sha <- parseSHA
--     _ <- endOfLine 
--     return sha

-- parseCommits :: Parser (Text -> (SHA, [SHA]))
-- parseCommits = do
--     shas <- many parseCommitLine
--     _ <- string "tree" >> string " " >> parseSHA 
--     _ <- endOfLine 
--     let parentCommits = [c [] | c <- shas]
--     return (, parentCommits)

-- resolveCommitsFromFind :: [(Text, Text)] -> [Commit]
-- resolveCommitsFromFind = 
--     let resolveCommitsFromFind' acc [] = acc
--         resolveCommitsFromFind' acc ((s,raw):ls) = 
--             case  maybeResult (parse parseSHA s) of
--                 Nothing -> resolveCommitsFromFind' acc ls
--                 Just sha -> case maybeResult (parse parseCommits raw) of
--                                 Just f -> f sha : resolveCommitsFromFind' acc ls
--                                 Nothing -> resolveCommitsFromFind' acc ls
--     in
--         resolveCommitsFromFind' []
