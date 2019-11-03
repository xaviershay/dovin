-- Dumping ground for things that haven't been thought through or tested yet.
module Dovin.Dump where

import Control.Arrow (second)
import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.HashMap.Strict as M
import qualified Data.Set as S
import System.Exit
import Data.List (groupBy, sort, sortBy)
import Data.Ord (comparing)
import Data.Function (on)
import Debug.Trace

import Dovin.Actions
import Dovin.Attributes
import Dovin.Builder
import Dovin.Formatting
import Dovin.Helpers
import Dovin.Monad
import Dovin.Types

gainLife :: Player -> Int -> GameMonad ()
gainLife player amount =
  modifying
    (life . at player . non 0)
    (+ amount)

loseLife :: Player -> Int -> GameMonad ()
loseLife player amount = gainLife player (-amount)

setLife :: Player -> Int -> GameMonad ()
setLife p n = assign (life . at p) (Just n)

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
