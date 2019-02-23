module Tap where

import TestPrelude.V2

test_Tap = testGroup "tap"
  [ prove "taps card in play" $ do
      withLocation Play $ addLand "Forest"
      tap "Forest"

      validate (matchAttribute tapped) "Forest"
  , prove "taps card in opponent's play" $ do
      withLocation Play $ addLand "Forest"
      tap "Forest"

      validate (matchAttribute tapped) "Forest"
  , prove "taps creature without summoning sickness" $ do
     withLocation Play $ addCreature (1, 1) "Goblin"
     tap "Goblin"
  , prove "taps creature with haste" $ do
     withLocation Hand $ withAttribute haste $ addCreature (1, 1) "Goblin"
     cast "" "Goblin" >> resolveTop
     tap "Goblin"
  , refute
      "requires card exists"
      "Card does not exist: Forest" $ do
        tap "Forest"
  , refute
      "requires untapped"
      "not has attribute tapped" $ do
        withAttribute tapped $ withLocation Play $ addLand "Forest"
        tap "Forest"
  , refute
      "requires in play"
      "in play" $ do
        withLocation Graveyard $ addLand "Forest"
        tap "Forest"
  , refute
     "requires creature not summoned"
     "does not have summoning sickness" $ do
       withLocation Hand $ addCreature (1, 1) "Soldier"
       cast "" "Soldier" >> resolveTop
       tap "Soldier"
  ]
