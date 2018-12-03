module Main where

import Dovin

import qualified Solutions.Example
import qualified Solutions.GuildsOfRavnicaPre2
import qualified Solutions.GuildsOfRavnica1
import qualified Solutions.GuildsOfRavnica3
import qualified Solutions.GuildsOfRavnica8
import qualified Solutions.GuildsOfRavnica9

main :: IO ()
main = do
  runVerbose Solutions.Example.solution
  runVerbose Solutions.GuildsOfRavnicaPre2.solution
  runVerbose Solutions.GuildsOfRavnica1.solution
  runVerbose Solutions.GuildsOfRavnica3.solution
  runVerbose Solutions.GuildsOfRavnica8.solution
  runVerbose Solutions.GuildsOfRavnica9.solution
