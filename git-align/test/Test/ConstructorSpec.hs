{-# LANGUAGE OverloadedStrings #-}
module Test.ConstructorSpec (constructorSpec) where
import Gitalign
    ( repoFromList,
      count,
      isChildOf,
      peekRepo,
      SHA(..),
      Repository(unRepo))
import Prelude (String, maximum, return, (==), not, zipWith, head, map, print, (++), Bool(True, False), Maybe (Just, Nothing), length, ($), (.), IO, fmap, and,Show (show), Eq ((/=)), (>), (>>=), (<=))
import Test.HUnit as H ( (~:), (~?=), Test(TestList) )
import Data.List (permutations)
import ASCII
import Test.Hspec ( hspec, describe, shouldBe, Spec )
import Test.Hspec.Contrib.HUnit qualified as HSC
import Test.Hspec.QuickCheck qualified  as HQ
import Data.Text (Text, pack)
import Data.List (nub)
import Data.Graph.Types (order, Graph(..))
import Test.QuickCheck
import Data.Maybe (isJust, isNothing)


constructorSpec :: IO ()
constructorSpec = hspec $ do
    simpleObjGraphConstruction
    repoCreationPropertyTests
    repoPropertyTests

            

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

repoCreationPropertyTests :: Spec
repoCreationPropertyTests = do
        describe "repoFromList property tests" $ do
            HQ.prop "size of repo == length of input list" $ do
                forAll shaMapGen $ \x -> count (repoFromList x) `shouldBe` length x
            

repoPropertyTests :: Spec
repoPropertyTests = do
    describe "repo property tests" $ do
        HQ.prop "peekRepo only returns Just a when repo is non-empty and vice versa" $ do
            \r -> case peekRepo r of
                Just _ -> count r > 0 `shouldBe` True
                Nothing -> count r == 0 `shouldBe` True

shaMapGen :: Gen [(SHA, [SHA])]
shaMapGen = do
        ss <- vectorOf 40 (arbitrary `suchThat` (not . (==) []) :: Gen String) :: Gen [String]
        let shas = map (SHA . pack) (nub ss)
        ys <- vectorOf (maximum [5, length ss]) (sublistOf shas)  :: Gen [[SHA]]
        (return  $ zipWith (\x y -> (x, y)) shas ys) :: Gen [(SHA, [SHA])]
