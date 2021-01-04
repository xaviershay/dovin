module CastFromZone where

import TestPrelude.V4

test_CastFromZone = testGroup "castFromZone"
  [ prove "places card on top of stack, spending mana" $ do
      withZone Graveyard $ addCreature (1, 1) "Zombie"
      addMana "B"
      castFromZone Graveyard "B" "Zombie"
      validate (matchZone Stack) "Zombie"
      validateBoardEquals (manaPoolFor Active) mempty

  , refute
      "requires mana be available"
      "Mana pool () does not contain (B)" $ do
        withZone Hand $ addCreature (1, 1) "Zombie"
        castFromZone Hand "B" "Zombie"

  , prove "can cast non-instant in second main" $ do
      withZone Hand $ addSorcery "Lava Spike"
      transitionTo SecondMain
      castFromZone Hand "" "Lava Spike"

  , refute
      "requires main phase for non-instant"
      "not in a main phase" $ do
        withZone Hand $ addSorcery "Lava Spike"
        transitionTo BeginCombat
        castFromZone Hand "" "Lava Spike"

  , refute
      "requires stack to be empty for non-instant"
      "stack is not empty" $ do
        withZone Hand $ addInstant "Shock"
        withZone Hand $ addSorcery "Lava Spike"

        castFromZone Hand "" "Shock"
        castFromZone Hand "" "Lava Spike"

  , prove "increases storm count if instant" $ do
      withZone Hand $ addInstant "Shock"
      castFromZone Hand "" "Shock"
      validateBoardEquals (counters . at "storm" . non 0) 1

  , prove "increases storm count if sorcery" $ do
      withZone Hand $ addSorcery "Lava Spike"
      castFromZone Hand "" "Lava Spike"

  , prove "does not increase storm count otherwise" $ do
      withZone Hand $ addArtifact "Mox Amber"
      castFromZone Hand "" "Mox Amber"
      validateBoardEquals (counters . at "storm" . non 0) 0
  ]
