{-# LANGUAGE OverloadedStrings #-}
module GitParsingSpec where

import Text.Parsec as P
import Git.Parsing
import Test.HUnit as H
import Data.Text as T
import System.IO
import Control.Monad (void)


completeGitParsingSpec :: IO ()
completeGitParsingSpec = 
    let gitParsingSpecList = TestList ["Parse git string literal" ~: parseGitStrLitSpec] in
    do 
        (counts', x) <- H.runTestText H.putTextToShowS gitParsingSpecList
        print counts'

parseGitStrLitSpec :: H.Test
parseGitStrLitSpec = 
    let input = "git commit" in
        case P.runParser parseGitString () "test data" input of
            Right x -> "git" H.~=? x
            Left err -> H.TestCase (H.assertFailure (show err))
