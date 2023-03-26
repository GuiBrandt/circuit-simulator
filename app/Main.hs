module Main (main) where

import qualified Text.Parsec.ByteString as P

import qualified CircuitSim.Parser as P
import CircuitSim.Data.Circuit (simulate0, simulate1, signals, SimStrat, State, CircuitSpec)
import CircuitSim.Data.Stimuli (Stimulus(..), simulateWithStimuli)

import qualified Data.HashMap.Strict as HM
import Data.Algorithm.Diff (getGroupedDiff, Diff)
import Data.Algorithm.DiffOutput (ppDiff)

import System.Environment (getArgs)
import System.FilePath.Glob (glob)
import System.FilePath ((</>))
import System.Exit (exitFailure)
import System.IO (stderr, openFile, Handle, IOMode (WriteMode), hPutStr, hPutStrLn, hClose)

import Control.Monad (forM, join, forM_, when)
import Control.Concurrent.Async (async, wait)

main :: IO ()
main = do
    args <- getArgs
    paths <- join <$> forM args glob
    handles <- forM paths $ async . runTest
    r <- concat <$> forM handles wait
    forM_ r $ \res -> do
        case res of
            Success f -> putStrLn $ f ++ " ok"
            Failure f d -> do
                hPutStrLn stderr $ f ++ " does not match expected:"
                hPutStrLn stderr $ ppDiff d
            Error e -> hPutStrLn stderr $ e ++ "\n"
    when (any failed r) exitFailure

data TestResult = Success String | Failure String [Diff [String]] | Error String

failed :: TestResult -> Bool
failed (Success _) = False
failed (Failure _ _) = True
failed (Error _) = True

runTest :: FilePath -> IO [TestResult]
runTest path = do
    let specFile = path </> "circuito.hdl"
    let stimuliFile = path </> "estimulos.txt"
    specAsync <- async $ P.parseFromFile P.circuit specFile
    stimuliAsync <- async $ P.parseFromFile P.stimuli stimuliFile
    eCircuit <- wait specAsync
    eStimuli <- wait stimuliAsync
    case (,) <$> eCircuit <*> eStimuli of
        Left e -> return [Error $ show e]
        Right (circuit, stimuli) -> do
            let sim = runSimulation circuit stimuli
            w0 <- async $ sim simulate0 (path </> "saida0.csv") (path </> "esperado0.csv")
            w1 <- async $ sim simulate1 (path </> "saida1.csv") (path </> "esperado1.csv")
            r0 <- wait w0
            r1 <- wait w1
            return [r0, r1]

runSimulation :: CircuitSpec -> [Stimulus] -> SimStrat -> FilePath -> FilePath -> IO TestResult
runSimulation circuit stimuli sim outFile expectedFile = do
    let s0 = HM.fromList [(k, False) | k <- signals circuit]
    let simulation = simulateWithStimuli stimuli sim circuit s0
    handle <- openFile outFile WriteMode
    hPrintSimulationAsync handle circuit simulation
    hClose handle
    aExpected <- async $ readFile expectedFile
    aOutput <- async $ readFile outFile
    expected <- lines <$> wait aExpected
    output <- lines <$> wait aOutput
    if expected == output then do
        return $ Success outFile
    else do
        let d = getGroupedDiff expected output
        return $ Failure outFile d

hPrintSimulationAsync :: Handle -> CircuitSpec -> [State] -> IO ()
hPrintSimulationAsync h spec sts = do
    hPutStr h "Tempo"
    forM_ (signals spec) $ \k -> hPutStr h (',':[k])
    hPutStrLn h ""
    go 0 sts
    where
        go :: Integer -> [State] -> IO ()
        go t (s:ss) = do
            hPutStr h (show t)
            forM_ (signals spec) $ \k ->
                hPutStr h (',':[if (HM.!) s k then '1' else '0'])
            hPutStrLn h ""
            go (t + 1) ss
        go _ [] = return ()
