{-# LANGUAGE OverloadedStrings #-}
module Test.ParsingSpec where

--import Test.Hspec qualified as H
import           Prelude (IO, ($), Maybe(Just),  Char, (<>), return)
import           Data.Char (isAlphaNum)
import           Data.Attoparsec.Text (maybeResult, parse)
import           Gitalign (shaFromDirectoryParser, parseSHA)
import           Test.Hspec (Spec, describe, hspec, shouldBe, it)
import           Data.Text (pack)
import qualified Test.Hspec.QuickCheck as HQ (prop)
import           Test.QuickCheck (Arbitrary (arbitrary), Gen, vectorOf, suchThat)

gitParsingSpec :: IO ()
gitParsingSpec = hspec $
    describe "it parses git objects from object directre correctly" $ do
        shaParsingSpec
        shaParsingQuickSpec

shaParsingSpec :: Spec
shaParsingSpec = do
    it "should parse sha w/ Unix-style path seperators" $ 
        let exampleShaPath = "da/39a3ee5e6b4b0d3255bfef95601890afd80709" in
            maybeResult (parse shaFromDirectoryParser exampleShaPath) `shouldBe` Just "da39a3ee5e6b4b0d3255bfef95601890afd80709"
    it "should parse sha w/ Windows-style path seperators" $
        let exampleShaPath = "da\\39a3ee5e6b4b0d3255bfef95601890afd80709" in
            maybeResult (parse shaFromDirectoryParser exampleShaPath) `shouldBe` Just "da39a3ee5e6b4b0d3255bfef95601890afd80709"

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
            
