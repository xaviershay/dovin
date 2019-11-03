module Mentor where

import TestPrelude.V2

test_Mentor = testGroup "mentor"
  [ prove "adds +1/+1 to target" $ do
      withLocation Play $ withAttributes [attacking, mentor] $
        addCreature (2, 2) "Legion Warboss"
      withLocation Play $ withAttributes [attacking] $
        addCreature (1, 1) "Goblin 1"

      triggerMentor "Goblin 1" "Legion Warboss"
      resolveMentor "Goblin 1" "Legion Warboss"

      c <- requireCard "Goblin 1" mempty

      unless (view cardStrength c == mkStrength (2, 2)) $
        throwError "Did not modify strength"

  , refute "requires attacking source" "has attribute attacking" $ do
      withLocation Play $ withAttributes [mentor] $
        addCreature (2, 2) "Legion Warboss"
      withLocation Play $ withAttributes [attacking] $
        addCreature (1, 1) "Goblin 1"

      triggerMentor "Goblin 1" "Legion Warboss"

  , refute "requires attacking target on trigger" "has attribute attacking" $ do
      withLocation Play $ withAttributes [mentor, attacking] $
        addCreature (2, 2) "Legion Warboss"
      withLocation Play $ withAttributes [] $
        addCreature (1, 1) "Goblin 1"

      triggerMentor "Goblin 1" "Legion Warboss"

  , refute "requires attacking target on resolution" "has attribute attacking" $ do
      withLocation Play $ withAttributes [mentor, attacking] $
        addCreature (2, 2) "Legion Warboss"
      withLocation Play $ withAttributes [attacking] $
        addCreature (1, 1) "Goblin 1"

      triggerMentor "Goblin 1" "Legion Warboss"
      loseAttribute attacking "Goblin 1"
      resolveMentor "Goblin 1" "Legion Warboss"

  , refute "requires mentor source" "has attribute mentor" $ do
      withLocation Play $ withAttributes [attacking] $
        addCreature (2, 2) "Legion Warboss"
      withLocation Play $ withAttributes [attacking] $
        addCreature (1, 1) "Goblin 1"

      triggerMentor "Goblin 1" "Legion Warboss"

  , refute "requires lesser power on trigger" "power < 1" $ do
      withLocation Play $ withAttributes [attacking, mentor] $
        addCreature (1, 2) "Legion Warboss"
      withLocation Play $ withAttributes [attacking] $
        addCreature (1, 1) "Goblin 1"

      triggerMentor "Goblin 1" "Legion Warboss"

  , refute "requires lesser power on resolution" "power < 2" $ do
      withLocation Play $ withAttributes [attacking, mentor] $
        addCreature (2, 2) "Legion Warboss"
      withLocation Play $ withAttributes [attacking] $
        addCreature (1, 1) "Goblin 1"

      triggerMentor "Goblin 1" "Legion Warboss"
      modifyStrength (1, 1) "Goblin 1"
      resolveMentor "Goblin 1" "Legion Warboss"
  ]
