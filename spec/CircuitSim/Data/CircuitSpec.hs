{-# OPTIONS_GHC -Wno-orphans #-}

module CircuitSim.Data.CircuitSpec (spec, CircuitWithState(..)) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import qualified Data.HashMap.Internal.Strict as HM
import Data.List.Extra (allSame)

import Control.Monad (replicateM)

import CircuitSim.Data.Circuit (simulate0, simulate1, signals, State, CircuitSpec(..), LogicGate (..))
import CircuitSim.Data.Util (untilRepeat)

data CircuitWithState = CircuitWithState CircuitSpec State deriving Show

instance Arbitrary CircuitSpec where
    arbitrary = sized $ \n -> do
        ks <- take n <$> shuffle ['A'..'Z']
        vs <- sized $ \m ->
            replicateM m $ oneof [
              And   <$> elements ks <*> elements ks
            , Or    <$> elements ks <*> elements ks
            , Not   <$> elements ks
            , Nand  <$> elements ks <*> elements ks
            , Nor   <$> elements ks <*> elements ks
            , Xor   <$> elements ks <*> elements ks
            ]
        return . Circuit . HM.fromList $ zip ks vs

instance Arbitrary CircuitWithState where
  arbitrary = do
    c <- arbitrary
    s <- do
        let ks = signals c
        vs <- mapM (const arbitrary) ks
        return . HM.fromList $ zip ks vs
    return $ CircuitWithState c s

spec :: Spec
spec = do
    describe "delay = 1" $ do
        prop "first simulated state is initial state" $
            \(CircuitWithState c s) -> simulate1 c s `shouldStartWith` [s]

        prop "signal set is stable" $
            \(CircuitWithState c s) -> (HM.keys <$> untilRepeat (simulate1 c s)) `shouldSatisfy` allSame

    describe "delay = 0" $ do
        prop "simulation is stable" $
            \(CircuitWithState c s) -> untilRepeat (simulate0 c s) `shouldSatisfy` allSame
