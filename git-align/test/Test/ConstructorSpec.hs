{-# LANGUAGE OverloadedStrings #-}
module Test.ConstructorSpec (constructorSpec) where
import Gitalign
    ( commitCount,
      fromCommitList,
      hasParent,
      numChildren,
      numParents,
      peekLatestCommit,
      Commit(CommitT, commitParents),
      Repository(unRepo) )
import Prelude (Bool(True), Maybe (Just, Nothing), length, ($), (.), IO, fmap, and)
import Test.HUnit as H ( (~:), (~?=), Test(TestList) )
import Test.Hspec ( hspec, describe, shouldBe, Spec )
import Test.Hspec.Contrib.HUnit qualified as HSC
import Test.Hspec.QuickCheck qualified  as HQ
import Data.List (nub)
import Data.Graph.Types (order)

constructorSpec :: IO ()
constructorSpec = hspec $ do
            simpleObjGraphConstruction
            repoPropertyTests
            

simpleObjGraphConstruction :: Spec 
simpleObjGraphConstruction = 
                                -- foo -- bar
                                --     \
                                --      ten -- excavate
    let repo = fromCommitList [CommitT "foo" ["bar", "ten"], CommitT "bar" [], CommitT "ten" ["excavate"], CommitT "excavate" []]
    in
    describe "constructs a repo from list" $
    do HSC.fromHUnitTest $
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

repoPropertyTests :: Spec
repoPropertyTests = do
        describe "basic repo prop tests" $ do
            HQ.prop "commitCount is same as number of distinct items" $
                \x -> (commitCount . fromCommitList) x `shouldBe` length (nub x) 
            HQ.prop "commitCount should behave exactly like order" $
                \r -> commitCount r `shouldBe` order (unRepo r)
            HQ.prop "commit a -> commit b iff a is a child of b" $
                \r -> 
                    let lc = peekLatestCommit r 
                    in
                        case lc of
                            Nothing -> commitCount r `shouldBe` 0
                            Just c -> and (fmap (hasParent r c) (commitParents c)) `shouldBe` True




