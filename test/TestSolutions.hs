module TestSolutions where

import TestPrelude

import qualified Solutions

test_Solutions = testGroup "Solutions" $
  map solutionTestCase Solutions.all

solutionTestCase (name, solution, _) = prove name solution
