module Counter where

import TestPrelude.V2

test_Case = testGroup "counter"
  [ prove "removes spell from stack" $ do
      as Opponent $ do
        withLocation Hand $ addInstant "Shock"
        cast "" "Shock"
        counter "Shock"

        validate
          (matchLocation (Opponent, Graveyard))
          "Shock"
        validateBoardEquals stack mempty
  , refute
      "requires card exists"
      "Card does not exist" $ do
        counter "Shock"
  , refute
      "requires card on stack"
      "on stack" $ do
        withLocation Hand $ addInstant "Shock"
        counter "Shock"
  , refute
      "cannot counter trigger"
      "not has attribute triggered or has attribute activated" $ do
        withLocation Play $ addArtifact "Dawn of Hope"
        trigger "Draw Card" "Dawn of Hope"
        counter "Draw Card"
  , refute
      "cannot counter activation"
      "not has attribute triggered or has attribute activated" $ do
        withLocation Play $ addArtifact "Dawn of Hope"
        activate "Create Soldier" "" "Dawn of Hope"
        counter "Create Soldier"
  ]
