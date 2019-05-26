module Main where

import Dovin.V2
import Dovin.Prelude
import Dovin.Monad

import qualified MTGTC

import System.Exit (exitFailure, exitSuccess)

main = do
  --let tape = "[s]sr"
  --let tape = "[p]fr"
  --let tape =  "crrffafafaffaffaaaaaaafaaaaaf[f]amamamc"
  --let tape = "rrffafafaf[f]amamamccccccccccc"
  let tape = "[s]"

  let (e, initialBoard, _) = runMonad emptyBoard (MTGTC.setup tape)
  putStrLn . MTGTC.tapeFormatter2 $ initialBoard
  _ <- foldM runCycle initialBoard [1..50]
  putStrLn "Done"

runCycle :: Board -> Int -> IO Board
runCycle board n = do
  let (e, newBoard, log) = runMonad board (MTGTC.stepCompute n)

  putStrLn ""
  putStrLn . MTGTC.tapeFormatter2 $ newBoard
 -- forM_ log $ \step -> do
 --   putStr $ show (view stepNumber step) <> ". "
 --   putStr $ view stepLabel step
 --   putStrLn . myFormatter . view stepState $ step

  case e of
    Left "won game" -> putStrLn "Won!" >> exitSuccess
    Left x -> putStrLn x >> exitFailure
    Right _ -> return newBoard

myFormatter =
  cardFormatter "tape" (MTGTC.matchAny (map matchAttribute MTGTC.tapeTypes))
