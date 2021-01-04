module Flashback where

import TestPrelude.V2

test_Flashback = testGroup "flashback"
  [ prove "casts from graveyard to stack" $ do
      as Opponent $ do
        withLocation Graveyard $ addInstant "Shock"
        addMana "R"
        flashback "R" "Shock"
        validate
          (matchLocation (Opponent, Stack) <> matchAttribute exileWhenLeaveStack)
          "Shock"
        validateBoardEquals (manaPoolFor Active) mempty
  , refute
      "requires card in graveyard"
      "in zone Graveyard" $ do
        withLocation Hand $ addInstant "Shock"
        flashback "" "Shock"
  ]
