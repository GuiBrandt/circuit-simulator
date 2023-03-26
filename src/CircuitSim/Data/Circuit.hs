{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}

module CircuitSim.Data.Circuit (
    LogicGate(..),
    CircuitSpec(..),
    State,
    SimStrat,
    signals,
    simulate0,
    simulate1
) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import Data.Foldable (Foldable(toList))

import CircuitSim.Data.Util (firstDuplicate)

data LogicGate t =
    And t t
    | Or t t
    | Not t
    | Nand t t
    | Nor t t
    | Xor t t
    deriving (Show, Functor, Foldable)

newtype CircuitSpec = Circuit (HM.HashMap Char (LogicGate Char)) deriving Show

type State = HM.HashMap Char Bool

type SimStrat = CircuitSpec -> State -> [State]

-- | Computa o sinal de saída de uma porta lógica.
--
-- >>> eval <$> [And x y | x <- [False, True], y <- [False, True]]
-- [False,False,False,True]
--
-- >>> eval <$> [Or x y | x <- [False, True], y <- [False, True]]
-- [False,True,True,True]
--
-- >>> eval <$> [Not x | x <- [False, True]]
-- [True,False]
--
-- >>> eval <$> [Nand x y | x <- [False, True], y <- [False, True]]
-- [True,True,True,False]
--
-- >>> eval <$> [Nor x y | x <- [False, True], y <- [False, True]]
-- [True,False,False,False]
--
-- >>> eval <$> [Xor x y | x <- [False, True], y <- [False, True]]
-- [False,True,True,False]
eval :: LogicGate Bool -> Bool
eval (And x y)  = x && y
eval (Or x y)   = x || y
eval (Not x)    = not x
eval (Nand x y) = not (x && y)
eval (Nor x y)  = not (x || y)
eval (Xor x y)  = (x || y) && not (x && y)

-- | Enumera os identificadores dos sinais de um circuito.
--
-- >>> let c = Circuit (HM.fromList [('A', And 'B' 'C'), ('B', Or 'C' 'D')])
-- >>> signals c
-- "ABCD"
signals :: CircuitSpec -> [Char]
signals (Circuit ss) = S.toAscList . S.fromList $ do
    (k, v) <- HM.toList ss
    k : toList v

-- | Simula um circuito com atraso um.
simulate1 :: SimStrat
simulate1 spec@(Circuit lg) st = let s' = next in st : simulate1 spec s'
    where
        next :: State
        next = (eval . fmap (st HM.!) <$> lg) <> st

-- | Simula um circuito com atraso zero.
simulate0 :: SimStrat
simulate0 spec s = repeat $ firstDuplicate (simulate1 spec s)
