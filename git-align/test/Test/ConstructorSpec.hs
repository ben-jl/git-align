{-# LANGUAGE OverloadedStrings #-}
module Test.ConstructorSpec (constructorSpec) where
import Test.HUnit as H
import Data.Text
import Data.Function ((&))
import Gitalign 
import qualified Data.HashMap.Strict as HM


constructorSpec :: IO ()
constructorSpec = 
    let testList = H.TestList [
            "Creates graph from list" & createsGraphFromList
            , "Creates graph from list 2" & createsGraphFromList
            ] :: H.Test
        in
    H.runTestTTAndExit testList
        
    
createsGraphFromList :: String -> Test
createsGraphFromList l = TestLabel l $ 
    let foo = [BlobT (SHA "foobar") (SHAOnly (SHA "foobar"))] :: [Blob] in 
    HM.lookup (BlobT (SHA "foobar") (SHAOnly (SHA "foobar"))) (contents $ objectRepoFromList foo) H.~=? Just [BlobT (SHA "foobar") (SHAOnly (SHA "foobar"))]

