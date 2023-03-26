module CircuitSim.Data.StimuliSpec (spec) where

import Test.Hspec
import Test.QuickCheck (Arbitrary (arbitrary), Gen, listOf, oneof, Property, Testable (property), suchThat)
import qualified Data.HashMap.Internal.Strict as HM

import CircuitSim.Data.Circuit (CircuitSpec, State, simulate0, simulate1, SimStrat)
import CircuitSim.Data.Stimuli (Stimulus (..), simulateWithStimuli)
import CircuitSim.Data.CircuitSpec (CircuitWithState(..))
import Test.Hspec.QuickCheck (prop)
import CircuitSim.Data.Util (untilRepeat)
import Data.List.Extra (allSame)

data CircuitStimuli = CircuitStimuli CircuitSpec State [Stimulus] deriving Show

instance Arbitrary CircuitStimuli where
  arbitrary = do
    CircuitWithState c st <- arbitrary :: Gen CircuitWithState
    ss <- listOf $ oneof [
            TimeSkip <$> arbitrary `suchThat` (> 0)
            , Assign <$> oneof (return <$> HM.keys st) <*> arbitrary
        ]
    return $ CircuitStimuli c st ss

simulationDuration :: SimStrat -> Property
simulationDuration sim = property $
    \(CircuitStimuli c st ss) ->
        let sts     = simulateWithStimuli ss sim c st
            mint    = sum [t | TimeSkip t <- ss]
            (l, r)  = splitAt mint sts
        in sts `shouldBe` l ++ untilRepeat r

spec :: Spec
spec = do
    describe "delay = 0" $ do
        prop "runs through all stimuli, stops on cycle" $ simulationDuration simulate0
        prop "signal set is stable" $
            \(CircuitStimuli c st ss) -> (HM.keys <$> simulateWithStimuli ss simulate0 c st) `shouldSatisfy` allSame

    describe "delay = 1" $ do
        prop "runs through all stimuli, stops on cycle" $ simulationDuration simulate1
        prop "signal set is stable" $
            \(CircuitStimuli c st ss) -> (HM.keys <$> simulateWithStimuli ss simulate1 c st) `shouldSatisfy` allSame
