module Activate where

import TestPrelude.V2

test_Activate = testGroup "activate"
  [ prove "adds activated card to stack" $ do
      withLocation Play $ addArtifact "Dawn of Hope"
      addMana "3W"
      activate "Create Soldier" "3W" "Dawn of Hope"
      validate
        (matchLocation (Active, Stack) <> matchAttribute activated)
        "Create Soldier"
      validateBoardEquals (manaPoolFor Active) mempty
      resolveTop
  , refute
      "requires card in play or graveyard"
      "in play or graveyard" $ do
        withLocation Hand $ addArtifact "Dawn of Hope"
        activate "" "" "Dawn of Hope"
  , refute
      "requires card controlled by actor"
      "has controller Opponent" $ do
        withLocation Hand $ addArtifact "Dawn of Hope"
        as Opponent $ activate "" "" "Dawn of Hope"
  ]
