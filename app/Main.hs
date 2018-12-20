module Main where

import Dovin

import qualified Solutions.Example
import qualified Solutions.ExampleEffects
import qualified Solutions.GuildsOfRavnicaPre2
import qualified Solutions.GuildsOfRavnica1
import qualified Solutions.GuildsOfRavnica3
import qualified Solutions.GuildsOfRavnica8
import qualified Solutions.GuildsOfRavnica9
import qualified Solutions.ExplorersOfIxalanContest
import qualified Solutions.UltimateMasters

main :: IO ()
main = do
  --runVerbose Solutions.Example.solution
  --runVerbose Solutions.GuildsOfRavnicaPre2.solution
  --runVerbose Solutions.GuildsOfRavnica1.solution
  --runVerbose Solutions.GuildsOfRavnica3.solution
  --run Solutions.GuildsOfRavnica8.formatter Solutions.GuildsOfRavnica8.solution
  --run Solutions.GuildsOfRavnica9.formatter Solutions.GuildsOfRavnica9.solution
  --run Solutions.ExplorersOfIxalanContest.formatter Solutions.ExplorersOfIxalanContest.solution
  run Solutions.UltimateMasters.formatter Solutions.UltimateMasters.solution
  --run Solutions.ExampleEffects.formatter Solutions.ExampleEffects.solution
