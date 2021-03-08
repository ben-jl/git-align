{-# LANGUAGE OverloadedStrings #-}
module Git.Parsing where
import Git.Types 
import qualified Text.Parsec as P
import qualified Text.Parsec.Combinator as PC
import Data.Text (Text)
import Data.Functor.Identity
import Data.String (fromString)
import Data.Char

type GitParser = P.Parsec Text ()

parseGitString :: GitParser Text
parseGitString = P.string "git" >> P.char ' ' >> pure "git" 

parseTreeElementType :: GitParser GitObjectType 
parseTreeElementType = P.choice [P.string "tree" >> pure Tree, P.string "blob" >> pure Blob]

parseTreeElementObject :: GitParser GitTreeObjectElement
parseTreeElementObject = do
    objMode <- P.count 6  P.anyChar
    P.spaces
    objType <- parseTreeElementType
    P.spaces 
    shaHash <- fromString <$> P.many1 P.alphaNum 
    P.spaces
    name <- fromString <$> P.many1 P.anyChar 
    return $ GitTreeObjectElement objMode objType shaHash name

parseTreeObject :: GitParser (Text -> GitTreeObject)
parseTreeObject = do
    elems <- P.sepBy1 parseTreeElementObject P.endOfLine 
    return $ \x -> GitTreeObjectT x elems

parseCommitObject :: GitParser (Text -> GitCommitObject)
parseCommitObject = do
    gTree <- P.string "tree" >> P.space >> P.many1 P.alphaNum
    P.endOfLine 
    let parseId = do
            P.string "author" P.<|> P.string "committer"
            P.space
            fullName <- P.many1 P.letter <> P.string " " <>  P.many1 P.letter
            P.space
            P.char '<'
            email <- P.many1 (P.alphaNum P.<|> P.oneOf ['_', '-', '&', '%', '*', '#', '!']) <> P.string "@" <> P.many1 (P.alphaNum P.<|> P.oneOf ['_', '-', '&', '%', '*', '#', '!']) <> P.string "." <> P.many1 P.letter
            P.char '>' 
            P.space
            P.many1 P.digit
            P.space
            P.char '-'
            P.many1 P.digit
            P.endOfLine
            return (fromString fullName :: Text, fromString email :: Text)
    auth <- parseId
    committer <- parseId
    P.satisfy  (not . isAlphaNum)
    message <- fromString <$> P.many1 P.anyChar
    P.eof 
    return $ \x -> GitCommitObjectT x (fromString gTree) (fst auth) (fst committer) message  


