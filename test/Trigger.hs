module Trigger where

import TestPrelude.V2

test_Trigger = testGroup "trigger"
  [ prove "adds triggered card to stack" $ do
      withLocation Play $ addArtifact "Dawn of Hope"
      trigger "Draw Card" "Dawn of Hope"
      validate
        (matchLocation (Active, Stack) <> matchAttribute triggered)
        "Draw Card"
      resolveTop
  , refute
      "requires card in play or graveyard"
      "in play or graveyard" $ do
        withLocation Hand $ addArtifact "Dawn of Hope"
        trigger "" "Dawn of Hope"
  , refute
      "requires card controlled by actor"
      "has controller Opponent" $ do
        withLocation Hand $ addArtifact "Dawn of Hope"
        as Opponent $ trigger "" "Dawn of Hope"
  ]
