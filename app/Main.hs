module Main where

import Control.Monad (forM_)
import Dovin

import qualified Solutions
import Solutions.WarOfTheSpark2

main :: IO ()
main = run formatter solution
--main = runAll

runAll =
  forM_ Solutions.all $ \(name, solution, formatter) ->
    run formatter solution
