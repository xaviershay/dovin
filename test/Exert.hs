module Exert where

import TestPrelude

test_Move = testGroup "exert"
  [ prove "exerts an attacking creature" $ do
      withLocation (Active, Play) $ addCreature (1, 1) "Bat"
      tap "Bat"
      exert "Bat"
      validate "Bat" $ matchAttribute exerted
  , refute
      "requires tapped creature"
      "has attribute tapped" $ do
        withLocation (Active, Play) $ addCreature (1, 1) "Bat"
        exert "Bat"
  ]
