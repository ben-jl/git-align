{-# LANGUAGE OverloadedStrings #-}
module Test.ConstructorSpec (constructorSpec) where
import Test.HUnit as H
import Data.Text
import Data.Function ((&))
import Gitalign 
import qualified Data.HashMap.Strict as HM
import Data.Functor
import Data.Maybe


constructorSpec :: IO ()
constructorSpec = 
    let testList = H.TestList [
            "Creates graph from list" H.~: createsGraphFromList
            ] :: H.Test
        in
    H.runTestTTAndExit testList
        
    
createsGraphFromList :: H.Test
createsGraphFromList = let input = [BlobT (SHA "foobar") (FullValue "cabaret"), BlobT (SHA "foobar") (FullValue "bar"), BlobT (SHA "cape") (FullValue "car")] 
                           hashmap = unRepo (objectRepoFromList input) in 
    TestList [
         maybe 0 Prelude.length (HM.lookup (SHA "foobar") hashmap) H.~=? 2,
         HM.lookup (SHA "cape") hashmap H.~=? Just [BlobT (SHA "cape") (FullValue "car")]

    ] 

