module Jumpstart where

import TestPrelude.V2

test_Cases = testGroup "jumpstart"
  [ prove "casts from graveyard to stack" $ do
      as Opponent $ do
        withLocation Graveyard $ addInstant "Shock"
        withLocation Hand $ addLand "Mountain"
        addMana "R"
        jumpstart "R" "Mountain" "Shock"
        validate
          (matchLocation (Opponent, Stack) <> matchAttribute exileWhenLeaveStack)
          "Shock"
        validate
          (matchLocation (Opponent, Graveyard))
          "Mountain"
        validateBoardEquals (manaPoolFor Active) mempty
  , refute
      "requires card in graveyard"
      "in zone Graveyard" $ do
        withLocation Hand $ addInstant "Shock"
        withLocation Hand $ addLand "Mountain"
        jumpstart "" "Mountain" "Shock"
  , refute
      "requires discard card in hand"
      "in location (Active,Hand)" $ do
        withLocation Hand $ addInstant "Shock"
        withLocation Play $ addLand "Mountain"
        jumpstart "" "Mountain" "Shock"
  ]
