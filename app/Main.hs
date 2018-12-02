module Main where

import Dovin

import qualified Solutions.GuildsOfRavnica8
import qualified Solutions.GuildsOfRavnica9

main :: IO ()
main = do
  runVerbose Solutions.GuildsOfRavnica8.solution
  runVerbose Solutions.GuildsOfRavnica9.solution
