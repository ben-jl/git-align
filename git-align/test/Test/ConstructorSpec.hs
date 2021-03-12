{-# LANGUAGE OverloadedStrings #-}
module Test.ConstructorSpec (constructorSpec) where
import Gitalign
    ( repoFromList,
      count,
      isChildOf,
      SHA(..),
      Repository)
import Prelude (Bool(True), Maybe (Just, Nothing), length, ($), (.), IO, fmap, and,Show (show), Eq ((/=)), (>))
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
-- constructorSpec = hspec $ do
--             simpleObjGraphConstruction
--             basicDirectAncestorTests
--             repoPropertyTests
--             otherwiseUnTouchedInstancesArentTrivial
            

simpleObjGraphConstruction :: Spec 
simpleObjGraphConstruction = 
                                -- foo -- bar
                                --     \
                                --      ten -- excavate
    let repo = repoFromList [(SHA "foo", [SHA "bar", SHA "ten"]), (SHA "bar", []), (SHA "ten", [SHA "excavate"]), (SHA "excavate", [])] 
    in
    describe "constructs a repo from list" $ 
        do HSC.fromHUnitTest $
            H.TestList [
                "it has four total items" H.~: count repo H.~?= 4,
                "it (foo) is child of ten" H.~: isChildOf repo (SHA "foo") (SHA "bar") H.~?= True, 
                "it (foo) is child of bar" H.~: isChildOf repo (SHA "foo" )  (SHA "bar" ) H.~?= True,
                "it (ten) is child of excavate" H.~: isChildOf repo (SHA "ten") (SHA "excavate") H.~?= True]

-- basicDirectAncestorTests :: Spec
-- basicDirectAncestorTests = 
--                                 -- foo -- bar
--                                 --     \
--                                 --      ten -- excavate
--     let barCommit = CommitT "bar" []
--         excavateCommit = CommitT "excavate" []
--         tenCommit = CommitT "ten" [excavateCommit]
--         fooCommit = CommitT "foo" [barCommit, tenCommit]
--     in
--     describe "gets correct ancestors from path search method" $ do
--         HSC.fromHUnitTest $
--             H.TestList [
--                 "it (foo) is returned by head" H.~: commitSHA fooCommit H.~?= "foo",
--                 "it (foo) is child of foo"   H.~: isDirectAncestor fooCommit fooCommit H.~?= Just [fooCommit],
--                 "it (foo) is child of bar"   H.~: isDirectAncestor fooCommit barCommit H.~?= Just [barCommit, fooCommit],
--                 "it (foo) is child of ten"   H.~: isDirectAncestor fooCommit tenCommit H.~?= Just [tenCommit, fooCommit],
--                 "it (foo) is grandchild of excavate" H.~: isDirectAncestor fooCommit excavateCommit H.~?= Just [excavateCommit, tenCommit, fooCommit],
--                 "it (bar) is not a child of foo" H.~: isDirectAncestor barCommit fooCommit H.~?= Nothing,
--                 "it (ten) is not a child of foo" H.~: isDirectAncestor tenCommit fooCommit H.~?= Nothing,
--                 "it (ten) is not a child of bar" H.~: isDirectAncestor tenCommit barCommit H.~?= Nothing,
--                 "it (bar) is not a child of ten" H.~: isDirectAncestor barCommit tenCommit H.~?= Nothing,
--                 "it (bar) is child of bar" H.~: isDirectAncestor barCommit barCommit H.~?= Just [barCommit],
--                 "it (ten) is child of ten" H.~: isDirectAncestor tenCommit tenCommit H.~?= Just [tenCommit],
--                 "it (bar) is not a child of excavate" H.~: isDirectAncestor barCommit excavateCommit H.~?= Nothing,
--                 "it (excavate) is a child of excavate" H.~: isDirectAncestor excavateCommit excavateCommit H.~?= Just [excavateCommit],
--                 "it (ten) is a child of excavate" H.~: isDirectAncestor tenCommit excavateCommit H.~?= Just [excavateCommit, tenCommit]
--                 ]

repoPropertyTests :: Spec
repoPropertyTests = do
        describe "basic repo prop tests" $ do
            HQ.prop "non empty list ==> non empty repo" $
                \x -> (count . repoFromList) x > -1 `shouldBe` True
--             HQ.prop "commitCount should behave exactly like order" $
--                 \r -> commitCount r `shouldBe` order (unRepo r)
--             HQ.prop "commit a -> commit b iff a is a child of b" $
--                 \r -> 
--                     let lc = peekLatestCommit r 
--                     in
--                         case lc of
--                             Nothing -> commitCount r `shouldBe` 0
--                             Just c -> and (fmap (hasParent r c) (commitParents c)) `shouldBe` True

-- otherwiseUnTouchedInstancesArentTrivial :: Spec
-- otherwiseUnTouchedInstancesArentTrivial = do
--     describe "Catchall for random instance declarations" $ do
--         HQ.prop "Show repo returns a non-empty string" $
--             \x -> show (x :: Repository) /= ""
--         HQ.prop "Show commit returns a non-empty string" $ 
--             \x -> show (x :: Commit) /= ""

