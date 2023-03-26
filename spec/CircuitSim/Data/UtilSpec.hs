module CircuitSim.Data.UtilSpec (spec) where
import Test.Hspec
import CircuitSim.Data.Util (firstDuplicate, untilRepeat, extend)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Data.List.Extra (dropEnd)

spec :: Spec
spec = do
    describe "firstDuplicate" $ do
        it "returns the first duplicate element in a list" $ do
            firstDuplicate [1, 1] `shouldBe` (1 :: Integer)
            firstDuplicate [1, 2, 1, 2] `shouldBe` (1 :: Integer)
            firstDuplicate [1, 2, 2, 1] `shouldBe` (2 :: Integer)

    describe "untilRepeat" $ do
        it "includes the first repetition" $ do
            untilRepeat [1 :: Integer, 1] `shouldBe` [1, 1]
            untilRepeat [1 :: Integer, 2, 1] `shouldBe` [1, 2, 1]
            untilRepeat [1 :: Integer, 2, 2, 1] `shouldBe` [1, 2, 2]

        it "preserves empty lists" $ do
            untilRepeat ([] :: [()]) `shouldBe` []

        prop "returns a prefix of the argument" $ \xs ->
            (xs :: [Int]) `shouldStartWith` untilRepeat xs

    describe "extend" $ do
        prop "preserves empty lists" $
            \(Fun _ f) -> extend ([] :: [Int]) f `shouldBe` []

        prop "extends lists" $ forAll (listOf1 arbitrary) $ \xs (Fun _ f) -> do
            let xs' = extend (xs :: [Int]) f
            xs' `shouldStartWith` dropEnd 1 xs
            drop (length xs - 1) xs' `shouldBe` f (last xs)
