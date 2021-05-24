module Draw where

import TestPrelude.V4

test_Draw = testGroup "draw"
  [ prove "moves cards from deck to hand" $ do
      withZone Deck $ do
        addLand "Forest"
        addLand "Mountain"
        addLand "Plains"

      draw 2

      validate (matchZone Deck) "Forest"
      validate (matchZone Hand) "Mountain"
      validate (matchZone Hand) "Plains"
  , refute
      "cannot draw from empty deck"
      "no card to draw" $ do
        draw 1
  ]
