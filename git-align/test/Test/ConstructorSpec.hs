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
import Prelude (Bool(True), Maybe (Just, Nothing), length, ($), (.), IO, fmap, and,Show (show), Eq ((/=)))
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
            otherwiseUnTouchedInstancesArentTrivial
            

simpleObjGraphConstruction :: Spec 
simpleObjGraphConstruction = 
                                -- foo -- bar
                                --     \
                                --      ten -- excavate
    let repo = fromCommitList [CommitT "foo" [CommitT "bar" [] , CommitT "ten" []], CommitT "bar" [], CommitT "ten" [CommitT "excavate" []], CommitT "excavate" []]
    in
    describe "constructs a repo from list" $
    do HSC.fromHUnitTest $
        H.TestList [
            "it (foo) is child of ten" H.~: hasParent repo (CommitT "foo" []) (CommitT "bar" []) H.~?= True, 
            "it (foo) is child of bar" H.~: hasParent repo (CommitT "foo" [])  (CommitT "bar" []) H.~?= True,
            "it (foo) has 0 children" H.~: numChildren repo (CommitT "foo" []) H.~?= 0,
            "it (foo) has 2 parents" H.~: numParents repo (CommitT "foo" []) H.~?= 2,
            "it (ten) is a child of excavate" H.~: hasParent repo (CommitT "ten" []) (CommitT "excavate" []) H.~?= True,
            "it (ten) has 1 child" H.~: numChildren repo (CommitT "ten" []) H.~?= 1, 
            "it (ten) has 1 parent" H.~: numParents repo (CommitT "ten" []) H.~?= 1, 
            "it (bar) has 0 parents" H.~: numParents repo (CommitT "bar" []) H.~?= 0,
            "it (bar) has 1 child" H.~: numChildren repo (CommitT "bar" []) H.~?= 1,
            "it (excavate) has 0 parents" H.~: numParents repo (CommitT "excavate" []) H.~?= 0,
            "it (excavate) has 1 child" H.~: numChildren repo (CommitT "excavate" []) H.~?= 1]

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

otherwiseUnTouchedInstancesArentTrivial :: Spec
otherwiseUnTouchedInstancesArentTrivial = do
    describe "Catchall for random instance declarations" $ do
        HQ.prop "Show repo returns a non-empty string" $
            \x -> show (x :: Repository) /= ""
        HQ.prop "Show commit returns a non-empty string" $ 
            \x -> show (x :: Commit) /= ""
