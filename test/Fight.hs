module Fight where

import TestPrelude

import Debug.Trace

test_Fight = testGroup "fight"
  [ prove "adds damage to both creatures" $ do
      withLocation (Active, Play) $ addCreature (2, 4) "Angel 1"
      withLocation (Active, Play) $ addCreature (3, 4) "Angel 2"

      fight "Angel 1" "Angel 2"
      validate "Angel 1" $ matchDamage 3
      validate "Angel 2" $ matchDamage 2
  , prove "fighting self adds double damage" $ do
      withLocation (Active, Play) $ addCreature (2, 4) "Angel 1"

      fight "Angel 1" "Angel 1"
      validate "Angel 1" $ matchDamage 4
  , refute
      "require first card to be in play"
      "in play" $ do
        withLocation (Active, Graveyard) $ addCreature (2, 4) "Angel 1"
        withLocation (Active, Play) $ addCreature (3, 4) "Angel 2"

        c <- requireCard "Angel 1" matchInPlay

        traceM (show $ view cardLocation c)

        fight "Angel 1" "Angel 2"
  , refute
      "require second card to be in play"
      "in play" $ do
        withLocation (Active, Play) $ addCreature (2, 4) "Angel 1"
        withLocation (Active, Graveyard) $ addCreature (3, 4) "Angel 2"

        fight "Angel 1" "Angel 2"
  , refute
      "require first card to be a creature"
      "has attribute creature" $ do
        withLocation (Active, Graveyard) $ addEnchantment "Angel 1"
        withLocation (Active, Play) $ addCreature (3, 4) "Angel 2"

        fight "Angel 1" "Angel 2"
  , refute
      "require second card to be a creature"
      "has attribute creature" $ do
        withLocation (Active, Play) $ addCreature (2, 4) "Angel 1"
        withLocation (Active, Graveyard) $ addEnchantment "Angel 2"

        fight "Angel 1" "Angel 2"
  ]
