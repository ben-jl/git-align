{-# LANGUAGE OverloadedStrings #-}
module Test.ConstructorSpec (constructorSpec) where

import Prelude
import Test.HUnit as H ( (~:), (~?=), Test(TestList) )
import Data.Hashable
import Test.Hspec ( hspec, describe, shouldBe, Spec )
import Test.Hspec.Contrib.HUnit qualified as HSC
import Test.Hspec.QuickCheck qualified  as HQ
import Test.QuickCheck


constructorSpec :: IO ()
constructorSpec = hspec $ do
    return ()
    -- simpleObjGraphConstruction
    --repoCreationPropertyTests
    --repoPropertyTests


-- shash :: String -> SHA
-- shash = hash    

-- simpleObjGraphConstruction :: Spec 
-- simpleObjGraphConstruction = do
--                                 -- foo -- bar
--                                 --     \
--                                 --      ten -- excavate
--     let repo = repoFromList [(shash "foo", [shash "bar", shash "ten"]), (shash "bar", []), (shash "ten", [shash "excavate"]), (shash "excavate", [])] 
--     describe "constructs a repo from list" $ do
--         HSC.fromHUnitTest $
--             H.TestList [
--                 "it has four total items" H.~: count repo H.~?= 4,
--                 "it (foo) is child of ten" H.~: isChildOf repo (shash "foo") (shash "bar") H.~?= True, 
--                 "it (foo) is child of bar" H.~: isChildOf repo (shash "foo" )  (shash "bar" ) H.~?= True,
--                 "it (ten) is child of excavate" H.~: isChildOf repo (shash "ten") (shash "excavate") H.~?= True]
    -- describe "detects ancestors in simple tree correctly" $ do
    --     HSC.fromHUnitTest $ 
    --         H.TestList [
    --             "it (foo) is child of foo" H.~: leastCommonAncestor repo (shash "foo") (shash "foo") H.~?= Just [shash "foo"],
    --             "it (foo) is child of bar" H.~: leastCommonAncestor repo (shash "foo") (shash "bar") H.~?= Just [shash "bar", shash "foo"],
    --             "it (foo) is child of ten" H.~: leastCommonAncestor repo (shash "foo") (shash "ten") H.~?= Just [shash "ten", shash "foo"],
    --             "it (foo) is child of excavate" H.~: leastCommonAncestor repo (shash "foo") (shash "excavate") H.~?= Just [shash "foo", shash "bar", shash "excavate"]
    --         ]

-- repoCreationPropertyTests :: Spec
-- repoCreationPropertyTests = do
--         describe "repoFromList property tests" $ do
--             HQ.prop "size of repo == length of input list" $ do
--                 forAll shaMapGen $ \x -> count (repoFromList x) `shouldBe` length x
--             HQ.prop "every member of snd (SHA, [SHA]) is related in repo" $ do
--                 forAll shaMapGen $ 
--                     \x -> let repo = repoFromList x in
--                         Prelude.all (\(y,ys) -> all (isChildOf repo y) ys) x `shouldBe` True
            

-- repoPropertyTests :: Spec
-- repoPropertyTests = do
--     describe "repo property tests" $ do
--         HQ.prop "peekRepo only returns Just a when repo is non-empty and vice versa" $ do
--             \r -> case peekRepo r of
--                 Just _ -> count r > 0 `shouldBe` True
--                 Nothing -> count r == 0 `shouldBe` True

-- shaMapGen :: Gen [(SHA, [SHA])]
-- shaMapGen = do
--         ss <- arbitrary :: Gen [Int]
--         let shas = nub ss
--         ys <- vectorOf (maximum [5, length ss]) (sublistOf shas)  :: Gen [[SHA]]
--         (return  $ zip shas ys) :: Gen [(SHA, [SHA])]
