module ModifyStrength where

import TestPrelude.V2

test_ModifyStrength = testGroup "modifyStrength"
  [ prove "modifies effective strength of creature" $ do
      withLocation Play $ addCreature (1, 1) "Soldier"
      modifyStrength (1, 2) "Soldier"

      c <- requireCard "Soldier" mempty

      unless (view cardStrength c == mkStrength (2, 3)) $
        throwError "Did not modify strength"
  , prove "strength modification reset when card leaves play" $ do
      withLocation Play $ addCreature (1, 1) "Soldier"
      modifyStrength (1, 2) "Soldier"

      withStateBasedActions $ moveTo Graveyard "Soldier"

      c <- requireCard "Soldier" mempty

      unless (view cardStrength c == mkStrength (1, 1)) $
        throwError "Did not reset strength"
  , prove "works with SBAs to graveyard creature for <= 0 toughness" $ do
      withLocation Play $ addCreature (1, 1) "Soldier"
      withStateBasedActions $ modifyStrength (0, -1) "Soldier"

      validate (matchLocation (Active, Graveyard)) "Soldier"
  , refute
      "requires in play"
      "in play" $ do
        withLocation Hand $ addCreature (1, 1) "Soldier"
        modifyStrength (0, -1) "Soldier"
  , refute
      "requires creature"
      "has attribute creature" $ do
        withLocation Hand $ addArtifact "Soldier"
        modifyStrength (0, -1) "Soldier"
  ]
