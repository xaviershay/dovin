-- Top-level run function for executing and printing solutions.
module Dovin.Runner
  ( run
  ) where

import System.Exit
import Data.List (groupBy, sort, sortBy)
import Data.Ord (comparing)
import Data.Function (on)

import Dovin.Prelude
import Dovin.Monad
import Dovin.Types

run :: (Step -> Formatter) -> GameMonad () -> IO ()
run formatter solution = do
  let (e, _, log) = runMonad emptyBoard solution

  let groupedSteps =
        groupBy ((==) `on` view stepFork) . sortBy (comparing $ view stepId ) $ log

  forM_ groupedSteps $ \steps -> do
    case view stepFork $ head steps of
      Just l -> do
        putStrLn ""
        putStrLn $ "=== ALTERNATIVE: " <> l
        putStrLn ""
      Nothing -> return ()

    forM_ steps $ \step -> do
      putStr $ show (view stepNumber step) <> ". "
      putStr $ view stepLabel step
      putStrLn . formatter step . view stepState $ step

  putStrLn ""
  case e of
    Left x -> do
      putStrLn "ERROR:"
      putStrLn x
      putStrLn ""
      System.Exit.exitFailure
    Right _ -> return ()
