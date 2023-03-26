module CircuitSim.Data.Util (firstDuplicate, untilRepeat, extend) where

import qualified Data.HashSet as HS
import Data.Hashable (Hashable)

-- | Retorna o primeiro elemento repetido em uma lista.
firstDuplicate :: Hashable a => [a] -> a
firstDuplicate = go HS.empty
    where
        go seen (s:ss)
            | s `HS.member` seen = s
            | otherwise = go (HS.insert s seen) ss
        go _ _ = undefined

-- | Retorna o prefixo de uma lista até a primeira repetição (incluindo a primeira repetição).
untilRepeat :: Hashable a => [a] -> [a]
untilRepeat = go HS.empty
    where
        go seen (x:xs)
            | x `HS.member` seen = [x]
            | otherwise = x : go (HS.insert x seen) xs
        go _ [] = []

-- | Estende uma lista aplicando uma função ao último elemento.
--
-- >>> extend [1, 2, 3] (\x -> replicate x x)
-- [1,2,3,3,3]
extend :: [a] -> (a -> [a]) -> [a]
extend [] _ = []
extend [x] f = f x
extend (x:xs) f = x : extend xs f
