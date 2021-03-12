{-# LANGUAGE OverloadedStrings #-}
module Test.ParsingSpec where

import           Prelude (IO, ($), Maybe(Just, Nothing), head, Char, (<>), return, String, Applicative (pure), (<$>), Int, length, print, (>>), (.))
import           Data.Char (isAlphaNum)
import           Data.Attoparsec.Text (maybeResult, parse, eitherResult)
import           Gitalign (shaFromDirectoryParser, parseSHA, parseCommitLine, parseCommits, Commit (commitParents))
import           Test.Hspec (Spec, describe, hspec, shouldBe, it)
import           Data.Text (pack, concat)
import qualified Test.Hspec.QuickCheck as HQ (prop)
import           Test.QuickCheck (Arbitrary (arbitrary), Gen, vectorOf, suchThat, oneof, chooseEnum)
import Data.Bool ( Bool(False, True) )
import Data.Either ( Either(Left, Right) )

gitParsingSpec :: IO ()
gitParsingSpec = hspec $
    describe "it parses git objects from object directory correctly" $ do
        shaParsingSpec
        commitLineSpec
        commitLineQuickSpec
        commitCatFileQuickSpec
        shaParsingQuickSpec

shaParsingSpec :: Spec
shaParsingSpec = do
    it "should parse sha w/ Unix-style path seperators" $ 
        let exampleShaPath = "da/39a3ee5e6b4b0d3255bfef95601890afd80709" in
            maybeResult (parse shaFromDirectoryParser exampleShaPath) `shouldBe` Just "da39a3ee5e6b4b0d3255bfef95601890afd80709"
    it "should parse sha w/ Windows-style path seperators" $
        let exampleShaPath = "da\\39a3ee5e6b4b0d3255bfef95601890afd80709" in
            maybeResult (parse shaFromDirectoryParser exampleShaPath) `shouldBe` Just "da39a3ee5e6b4b0d3255bfef95601890afd80709"

commitLineSpec :: Spec
commitLineSpec = do
    it "should parse a line starting w/ commit" $
        let example = "commit cac0cab538b970a37ea1e769cbbde608743bc96d\n"
            res = maybeResult (parse parseCommitLine example)  in
                res `shouldBe` Just (pack "cac0cab538b970a37ea1e769cbbde608743bc96d")

commitLineQuickSpec :: Spec
commitLineQuickSpec = do
    HQ.prop "should parse every line starting with commit" $ do
        prefix <- oneof [pure  "commit", arbitrary :: Gen String]
        sha <- pack <$> (vectorOf 40 (arbitrary `suchThat` isAlphaNum :: Gen Char) :: Gen String)
        let line = pack prefix <> " " <> sha <> "\n"
        let res = maybeResult (parse parseCommitLine line)
        return $ case prefix of
                    "commit" -> res `shouldBe` Just sha
                    _ -> res `shouldBe` Nothing

commitCatFileQuickSpec :: Spec
commitCatFileQuickSpec = do
    HQ.prop "should parse commits w/ variable parents" $ do
        countParents <- chooseEnum (0, 10) :: Gen Int
        commits <- vectorOf countParents (vectorOf 40 (arbitrary `suchThat` isAlphaNum :: Gen Char))
        sha <- vectorOf 40 (arbitrary `suchThat` isAlphaNum :: Gen Char)
        let commitLine = concat (pack . (\x -> "commit " <> x <> "\n") <$> commits) <> "tree cac0cab538b970a37ea1e769cbbde608743bc96d\n"
        return $ case eitherResult (parse parseCommits commitLine) of 
                    Right f  -> length (commitParents (f (pack sha))) `shouldBe` countParents
                    Left err -> print err >> (True `shouldBe` False)
    HQ.prop "should parse commits w/ author lines" $ do
        countParents <- chooseEnum (0, 10) :: Gen Int
        commits <- vectorOf countParents (vectorOf 40 (arbitrary `suchThat` isAlphaNum :: Gen Char))
        sha <- vectorOf 40 (arbitrary `suchThat` isAlphaNum :: Gen Char)
        let commitLine = concat (pack . (\x -> "commit " <> x <> "\n") <$> commits) <> "tree cac0cab538b970a37ea1e769cbbde608743bc96d\nAuthor: Scott Chacon <schacon@gmail.com>\n"
        return $ case eitherResult (parse parseCommits commitLine) of
                    Right f  -> length (commitParents (f (pack sha))) `shouldBe` countParents
                    Left err -> print err >> (True `shouldBe` False)
    HQ.prop "should parse commits w/ author lines and date" $ do
        countParents <- chooseEnum (0, 10) :: Gen Int
        commits <- vectorOf countParents (vectorOf 40 (arbitrary `suchThat` isAlphaNum :: Gen Char))
        sha <- vectorOf 40 (arbitrary `suchThat` isAlphaNum :: Gen Char)
        let commitLine = concat (pack . (\x -> "commit " <> x <> "\n") <$> commits) <> "tree cac0cab538b970a37ea1e769cbbde608743bc96d\nAuthor: Scott Chacon <schacon@gmail.com>\nDate:   Fri May 22 18:14:29 2009 -0700\n"
        return $ case eitherResult (parse parseCommits commitLine) of
                    Right f  -> length (commitParents (f (pack sha))) `shouldBe` countParents
                    Left err -> print err >> (True `shouldBe` False)
    HQ.prop "should parse commits w/ author lines and date and message" $ do
        countParents <- chooseEnum (0, 10) :: Gen Int
        commits <- vectorOf countParents (vectorOf 40 (arbitrary `suchThat` isAlphaNum :: Gen Char))
        sha <- vectorOf 40 (arbitrary `suchThat` isAlphaNum :: Gen Char)
        let commitLine = concat (pack . (\x -> "commit " <> x <> "\n") <$> commits) <> "tree cac0cab538b970a37ea1e769cbbde608743bc96d\nAuthor: Scott Chacon <schacon@gmail.com>\nDate:   Fri May 22 18:14:29 2009 -0700\n\nFirst line\nSecond line"
        return $ case eitherResult (parse parseCommits commitLine) of
                    Right f  -> length (commitParents (f (pack sha))) `shouldBe` countParents
                    Left err -> print err >> (True `shouldBe` False)
    HQ.prop "should parse commits w/ variable parents" $ do
        countParents <- chooseEnum (0, 10) :: Gen Int
        commits <- vectorOf countParents (vectorOf 40 (arbitrary `suchThat` isAlphaNum :: Gen Char))
        sha <- vectorOf 40 (arbitrary `suchThat` isAlphaNum :: Gen Char)
        let commitLine = concat (pack . (\x -> "commit " <> x <> "\n") <$> commits) <> "tree cac0cab538b970a37ea1e769cbbde608743bc96d\n"
        return $ case eitherResult (parse parseCommits commitLine) of 
                    Right f -> let parentsLength = length (commitParents (f (pack sha))) in case parentsLength of
                                    0 -> 0 `shouldBe` countParents
                                    _ -> Just (length (commitParents . head $ commitParents (f (pack sha)))) `shouldBe` Just 0
                    Left err -> print err >> (True `shouldBe` False)

shaParsingQuickSpec :: Spec
shaParsingQuickSpec = do 
    describe "automated property testing of SHA parser from directory" $ do
        HQ.prop "parse result matches input w/out strings (Unix path seperators)"  $  do
            firstTwo <- vectorOf 2 (arbitrary `suchThat` isAlphaNum :: Gen Char) 
            remaining <- vectorOf 38 (arbitrary `suchThat` isAlphaNum :: Gen Char)
            let pathString =  pack firstTwo <> "/" <> pack remaining
            let expectedOutput = pack firstTwo <> pack remaining
            return $ maybeResult (parse shaFromDirectoryParser pathString) `shouldBe` Just expectedOutput
        HQ.prop "parse result matches input w/out strings (Windows path seperators)"  $ do
            firstTwo <- vectorOf 2 (arbitrary `suchThat` isAlphaNum :: Gen Char)
            remaining <- vectorOf 38 (arbitrary `suchThat` isAlphaNum :: Gen Char)
            let pathString =  pack firstTwo <> "\\" <> pack remaining
            let expectedOutput = pack firstTwo <> pack remaining
            return $ maybeResult (parse shaFromDirectoryParser pathString) `shouldBe` Just expectedOutput
    describe "automated property testing of SHA parser (no directory)" $ do 
        HQ.prop "all 40 character alphanumberic strings pass" $ do
            x <- vectorOf 40 (arbitrary `suchThat` isAlphaNum :: Gen Char)
            return $ maybeResult (parse parseSHA (pack x)) `shouldBe` Just (pack x)
            
