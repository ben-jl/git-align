{-# LANGUAGE OverloadedStrings #-}
module Test.ConstructorSpec (constructorSpec) where
import Gitalign
import Test.HUnit as H
import Test.Hspec
import qualified Test.Hspec.Contrib.HUnit as HSC
import qualified Test.QuickCheck as Q
import qualified Test.Hspec.QuickCheck as HQ
import Data.Text
import Data.String
import Data.List
import Data.Graph.DGraph (DGraph)
import Data.Graph.Types ( Graph(toList, edgePairs, fromList, insertEdgePairs, empty, order) )
import Data.Bifunctor

constructorSpec :: IO ()
constructorSpec = hspec $ do
    describe "standard tests" $
        do 
            simpleObjGraphConstruction
    describe "property tests" $
        do
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
repoPropertyTests = 
    describe "repo util functions satisfy expected properties" $
        describe "fromCommitList satisifies expected properties" $ do
            HQ.prop "order of repo is same as number of distinct items in list" $
                \x ->
                    let lstSizeCheck l =
                            case Prelude.length (nub l) of
                                x | x == 0 -> "Empty"
                                x | x <= 5 -> "Small"
                                x | x >= 5 && x <= 15 -> "Medium"
                                _ -> "Large"
                    in
                    Q.tabulate "List size" [show (lstSizeCheck x)] $
                        (commitCount . fromCommitList) x `shouldBe` Prelude.length (nub x) 
            HQ.prop "commitCount should behave exactly like order . unrepo" $
                \r ->
                    let repoSizeStr re = 
                            case commitCount re of 
                                z | z == 0 -> "Empty"
                                z | z <= 5 -> "Small"
                                z | z <= 15 -> "Medium"
                                _ -> "Large" 
                    in
                    Q.tabulate "Number of parents" [repoSizeStr r] $
                        commitCount r `shouldBe` order (unRepo r)
            HQ.prop "commit a -> commit b iff a is a child of b" $
                \r -> 
                    let lc = popLatestCommit r 
                        sizeString = case Prelude.length (maybe [] commitParents lc) of
                            z | z == 0 -> "No parents"
                            z | z == 1 -> "Exactly one parent"
                            z | z <= 5 -> "Less than five parents"
                            _ -> "Many parents"
                    in
                    Q.tabulate "Number of parents" [sizeString] $
                        Q.cover 75 (sizeString /= "No parents") "non-trivial" $
                                case lc of
                                    Nothing -> commitCount r `shouldBe` 0
                                    Just c -> and (fmap (hasParent r c) (commitParents c)) `shouldBe` True

instance Q.Arbitrary Repository where
    arbitrary = do
        strList <- Q.listOf (Q.arbitrary `Q.suchThat` (\c -> Prelude.length c <= 8) :: Q.Gen String) 
        let subseqs = Data.List.take (Prelude.length strList) (permutations strList)
        let commits = Data.List.zipWith (\x y -> CommitT (fromString x) (fromString <$> y)) strList subseqs
        return $ fromCommitList commits

instance Q.Arbitrary Commit where
    arbitrary = do
        textList <- Q.listOf (Q.arbitrary `Q.suchThat` (/=) []) `Q.suchThat` (\c -> Prelude.length c <= 5) :: Q.Gen [String]
        currSha <- Q.arbitrary `Q.suchThat` (/=) [] :: Q.Gen String
        return $ CommitT (fromString currSha) (CommitT <$> Prelude.map fromString textList <*> [])


