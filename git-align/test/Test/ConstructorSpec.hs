{-# LANGUAGE OverloadedStrings #-}
module Test.ConstructorSpec (constructorSpec) where
import Test.HUnit as H
import Data.Text
import Gitalign


constructorSpec :: IO ()
constructorSpec = H.runTestTTAndExit
    simpleObjGraphConstruction

simpleObjGraphConstruction :: H.Test 
simpleObjGraphConstruction = 
                                -- foo -- bar
                                --     \
                                --      ten -- excavate
    let repo = fromCommitList [CommitT "foo" ["bar", "ten"], CommitT "bar" [], CommitT "ten" ["excavate"], CommitT "excavate" []] 
        numParents c = Prelude.length (parents c repo) in
    H.TestList [
            "it (foo) has the correct number of parents" H.~: numParents (CommitT "foo" [])  H.~?= 2, 
            "it (ten) has the correct number of parents" H.~: numParents (CommitT "ten" []) H.~?= 1,
            "it (bar) has the correct number of parents" H.~: numParents (CommitT "bar" []) H.~?= 0,
            "it (excavate) has the correct number of parents" H.~: numParents (CommitT "excavate" []) H.~?= 0 ]
