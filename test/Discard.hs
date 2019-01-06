module Discard where

import TestPrelude.V2

test_Cases = testGroup "jumpstart"
  [ prove "discards card from hand to graveyard" $ do
      as Opponent $ do
        withLocation Hand $ addLand "Mountain"
        discard "Mountain"
        validate
          (matchLocation (Opponent, Graveyard))
          "Mountain"
  , refute
      "requires card in hand"
      "in location (Active,Hand)" $ do
        withLocation Play $ addLand "Mountain"
        discard "Mountain"
  ]
