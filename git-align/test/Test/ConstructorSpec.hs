{-# LANGUAGE OverloadedStrings #-}
module Test.ConstructorSpec (constructorSpec) where
import Test.HUnit as H
import Data.Text
import Gitalign
import Data.Graph.Visualize as V
import Data.Graph.Connectivity
import Data.Graph.DGraph

constructorSpec :: IO ()
constructorSpec = H.runTestTTAndExit
    simpleObjGraphConstruction

simpleObjGraphConstruction :: H.Test 
simpleObjGraphConstruction = 
                                -- foo -- bar
                                --     \
                                --      ten -- excavate
    let repo = fromCommitList [CommitT "foo" ["bar", "ten"], CommitT "bar" [], CommitT "ten" ["excavate"], CommitT "excavate" []] in
    H.TestList [
            "it (foo) is child of ten" H.~: hasParent repo "foo" "bar" H.~?= True, 
            "it (foo) is child of bar" H.~: hasParent repo "foo" "bar" H.~?= True,
            "it (foo) has 0 children" H.~: numChildren repo "foo" H.~?= 0,
            "it (foo) has 2 parents" H.~: numParents repo "foo" H.~?= 2,
            "it (ten) is a child of excavate" H.~: hasParent repo "ten" "excavate" H.~?= True,
            "it (ten) has 1 child" H.~: numChildren repo "ten" H.~?= 1, 
            "it (ten) has 1 parent" H.~: numParents repo "ten" H.~?= 1, 
            "it (bar) has 0 parents" H.~: numParents repo "bar" H.~?= 0,
            "it (bar) has 1 child" H.~: numChildren repo "bar" H.~?= 1,
            "it (excavate) has 0 parents" H.~: numParents repo "excavate" H.~?= 0,
            "it (excavate) has 1 child" H.~: numChildren repo "excavate" H.~?= 1]

