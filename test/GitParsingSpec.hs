{-# LANGUAGE OverloadedStrings #-}
module GitParsingSpec where

import Text.Parsec as P
import Git.Parsing
import Git.Types
import Test.HUnit as H
import Data.Text as T
import System.IO
import Control.Monad (void)

completeGitParsingSpec :: IO ()
completeGitParsingSpec = 
    let gitParsingSpecList = TestList [
            "Parse git string literal" ~: parseGitStrLitSpec, 
            "Parse git obj type" ~: parseGitTreeObjTypeSpec,
            "Parse git tree object element" ~: parseGitTreeObjTypeSpec,
            "Parse git tree object" ~: parseTreeSpec,
            "Parse git commit object" ~: parseCommitSpec,
            "Parse git commit object - w/ symbols" ~: parseCommitSpec2
            ] in
    do 
        x <- H.runTestText (H.putTextToHandle stdout True) gitParsingSpecList
        return ()

parseGitStrLitSpec :: H.Test
parseGitStrLitSpec = 
    let input = "git commit" in
        case P.runParser parseGitString () "test data" input of
            Right x -> "git" H.~=? x
            Left err -> H.TestCase (H.assertFailure (show err))

parseGitTreeObjTypeSpec :: H.Test
parseGitTreeObjTypeSpec = 
    TestList $ Prelude.map 
        (\(x,y) -> either (H.TestCase . H.assertFailure . show) (x H.~=?)  (P.runParser parseTreeElementType () "test data" y)) 
        [(Blob, "blob"), (Tree, "tree")]
            
parseTreeElementObjectSpec :: H.Test
parseTreeElementObjectSpec = 
    let input1 = "100644 blob a906cb2a4a904a152e80877d4088654daad0c859 README.md"
        input2 = "040000 tree 99f1a6d12cb4b6f19c8655fca46c3ecf317074e0      lib"
        expect1 = GitTreeObjectElement "100644" Blob "a906cb2a4a904a152e80877d4088654daad0c859" "README.md"
        expect2 = GitTreeObjectElement "040000" Tree "99f1a6d12cb4b6f19c8655fca46c3ecf317074e0" "lib"
    in
        TestList $ makeTest <$> [input1, input2] <*> [expect1, expect2] where
    makeTest str expected = either (H.TestCase . H.assertFailure . show) (expected H.~=?) (P.runParser parseTreeElementObject () "test data" str)

parseTreeSpec :: H.Test
parseTreeSpec = 
    let sha = "47c6340d6459e05787f644c2447d2595f5d3a54b"
        input = "100644 blob a906cb2a4a904a152e80877d4088654daad0c859 README.md\n040000 tree 99f1a6d12cb4b6f19c8655fca46c3ecf317074e0      lib"
        expected = GitTreeObjectT sha [
            GitTreeObjectElement "100644" Blob "a906cb2a4a904a152e80877d4088654daad0c859" "README.md",
            GitTreeObjectElement "040000" Tree "99f1a6d12cb4b6f19c8655fca46c3ecf317074e0" "lib"
            ]
    in
        either (H.TestCase . H.assertFailure . show) 
        (\x -> H.TestCase <$> H.assert $ (gitTreeObjSha256 expected == gitTreeObjSha256  (x sha)))
        (P.runParser parseTreeObject () "test data" input)

parseCommitSpec :: H.Test
parseCommitSpec = 
    let sha = "47c6340d6459e05787f644c2447d2595f5d3a54b"
        result = P.runParser parseCommitObject () "test data" commitMsg
        expected = GitCommitObjectT sha "d8329fc1cc938780ffdd9f94e0d364e0ea74f579" "Scott Chacon" "Scott Chacon" "First commit"
    in
        either (H.TestCase . H.assertFailure . show) (\x -> expected H.~=? x sha) result

parseCommitSpec2 :: H.Test
parseCommitSpec2 =
    let sha = "47c6340d6459e05787f644c2447d2595f5d3a54b"
        result = P.runParser parseCommitObject () "test data" commitMsg2
        expected = GitCommitObjectT sha "d8329fc1cc938780ffdd9f94e0d364e0ea74f579" "Scott Chacon" "Scott Chacon" "First commit\n1234 %$^"
    in
        either (H.TestCase . H.assertFailure . show) (\x -> expected H.~=? x sha) result


commitMsg :: Text
commitMsg = "tree d8329fc1cc938780ffdd9f94e0d364e0ea74f579\n\
\author Scott Chacon <schacon@gmail.com> 1243040974 -0700\n\
\committer Scott Chacon <schacon@gmail.com> 1243040974 -0700\n\
\\n\
\First commit"

commitMsg2 :: Text
commitMsg2 = "tree d8329fc1cc938780ffdd9f94e0d364e0ea74f579\n\
\author Scott Chacon <scha_con@gmail3.com> 1243040974 -0700\n\
\committer Scott Chacon <scha_con@gmail3.com> 1243040974 -0700\n\
\\n\
\First commit\n\
\1234 %$^"
