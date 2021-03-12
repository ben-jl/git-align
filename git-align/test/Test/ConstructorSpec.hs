{-# LANGUAGE OverloadedStrings #-}
module Test.ConstructorSpec (constructorSpec) where
import Gitalign
    ( repoFromList,
      count,
      isChildOf,
      peekRepo,
      SHA(..),
      Repository)
import Prelude (String, return, (==), not, zipWith, head, map, print, (++), Bool(True, False), Maybe (Just, Nothing), length, ($), (.), IO, fmap, and,Show (show), Eq ((/=)), (>), (>>=), (<=))
import Test.HUnit as H ( (~:), (~?=), Test(TestList) )
import Data.List (permutations)
import Test.Hspec ( hspec, describe, shouldBe, Spec )
import Test.Hspec.Contrib.HUnit qualified as HSC
import Test.Hspec.QuickCheck qualified  as HQ
import Data.Text (pack)
import Data.List (nub)
import Data.Graph.Types (order)
import Test.QuickCheck
import Data.Maybe (isJust, isNothing)


constructorSpec :: IO ()
constructorSpec = hspec $ do
    simpleObjGraphConstruction
    repoCreationPropertyTests
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

repoCreationPropertyTests :: Spec
repoCreationPropertyTests = do
        describe "repoFromList property tests" $ do
            HQ.prop "size of repo == length of input list" $ do
                forAll shaMapGen $ \x -> count (repoFromList x) `shouldBe` length (nub x)

repoPropertyTests :: Spec
repoPropertyTests = do
    describe "repo property tests" $ do
        HQ.prop "peekRepo only returns Just a when repo is non-empty and vice versa" $ do
            \r -> case peekRepo r of
                Just _ -> count r > 0 `shouldBe` True
                Nothing -> count r == 0 `shouldBe` True

shaMapGen :: Gen [(SHA, [SHA])]
shaMapGen = do
        ss <- listOf (arbitrary `suchThat` (not . (==) []) :: Gen String) `suchThat` (\x -> length x <= 10)  :: Gen [String]
        ys <- vectorOf (length ss) (sublistOf ss) :: Gen [[String]]
        (return  $ zipWith (\x y -> (SHA (pack x), map (SHA . pack) y)) ss ys) :: Gen [(SHA, [SHA])]
