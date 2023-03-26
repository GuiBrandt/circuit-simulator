module CircuitSim.Data.Stimuli (Stimulus(..), simulateWithStimuli) where

import CircuitSim.Data.Circuit (SimStrat)

import qualified Data.HashMap.Strict as HM
import CircuitSim.Data.Util (untilRepeat, extend)

data Stimulus = TimeSkip Int | Assign Char Bool deriving Show

-- | Executa uma simulação com estímulos
simulateWithStimuli :: [Stimulus] -> SimStrat -> SimStrat
simulateWithStimuli [] sim spec st = untilRepeat $ sim spec st
simulateWithStimuli (Assign k v : ss) sim spec st =
    let st' = HM.insert k v st
    in simulateWithStimuli ss sim spec st'
simulateWithStimuli (TimeSkip t : ss) sim spec st =
    let sts = take (t + 1) $ sim spec st
    in extend sts $ simulateWithStimuli ss sim spec
