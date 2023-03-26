{-# LANGUAGE OverloadedStrings #-}

module CircuitSim.Parser (circuit, stimuli, CircuitSpec(..)) where

import qualified Data.HashMap.Strict as HM
import qualified Text.Parsec.ByteString as PB
import qualified Text.Parsec as P

import Text.Parsec ((<?>), (<|>))
import Control.Monad (void)

import CircuitSim.Data.Stimuli (Stimulus(..))
import CircuitSim.Data.Circuit (CircuitSpec(..), LogicGate(..))

-- | Lê uma especificação de circuito.
--
-- >>> P.parse circuit "" "A = AND B C\nB = OR C D"
-- Right (Circuit (fromList [('A',And 'B' 'C'),('B',Or 'C' 'D')]))
circuit :: PB.Parser CircuitSpec
circuit = Circuit . HM.fromList <$> P.many1 entry <?> "circuit specification"
    where
        entry = gate <* P.many (P.char ' ') <* (void P.endOfLine <|> P.eof)
        gate = (,) <$> ref <* P.spaces <* P.char '=' <* P.spaces <*> signal' <?> "signal definition"
            where
                signal' = P.choice [
                    P.string "AND" *> P.spaces *> (And <$> ref <* P.spaces <*> ref)
                    , P.string "OR" *> P.spaces *> (Or <$> ref <* P.spaces <*> ref)
                    , P.try (P.string "NOT") *> P.spaces *> (Not <$> ref)
                    , P.try (P.string "NAND") *> P.spaces *> (Nand <$> ref <* P.spaces <*> ref)
                    , P.try (P.string "NOR") *> P.spaces *> (Nor <$> ref <* P.spaces <*> ref)
                    , P.string "XOR" *> P.spaces *> (Xor <$> ref <* P.spaces <*> ref)
                    ]
                    <?> "logic gate"

-- | Lê uma especificação de entrada de estímulos.
--
-- >>> P.parse stimuli "" "FG = 01\n+1\nGHI = 110"
-- Right [Assign 'F' False,Assign 'G' True,TimeSkip 1,Assign 'G' True,Assign 'H' True,Assign 'I' False]
stimuli :: PB.Parser [Stimulus]
stimuli = concat <$> P.many1 entry <?> "stimuli"
    where
        entry = (stimulus <|> (:[]) <$> timeskip) <* P.many (P.char ' ') <* (void P.endOfLine <|> P.eof)
        stimulus = do
            refs <- P.many1 ref
            P.spaces <* P.char '=' *> P.spaces
            bits <- P.many1 bit
            if length refs == length bits
                then return $ uncurry Assign <$> zip refs bits
                else fail "mismatched signal assignments"
        timeskip = TimeSkip . read <$> (P.char '+' *> P.many1 P.digit) <?> "time skip"
        bit = (True <$ P.char '1') <|> (False <$ P.char '0') <?> "bit literal"

ref :: PB.Parser Char
ref = P.oneOf ['A'..'Z'] <?> "signal references"
