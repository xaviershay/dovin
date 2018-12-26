module Main where

import Control.Monad (forM_)
import Dovin

import qualified Solutions

main :: IO ()
main = runAll

runAll =
  forM_ Solutions.all $ \(name, solution, formatter) ->
    run formatter solution
