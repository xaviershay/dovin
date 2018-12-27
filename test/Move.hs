module Move where

import TestPrelude

test_Move = testGroup "move/moveTo"
  [ prove "moves card from one location to another" $ do
      withLocation (Active, Hand) $ addLand "Forest"
      move (Active, Hand) (Active, Exile) "Forest"

      validate "Forest" $ matchLocation (Active, Exile)
  , prove "does not allow tokens to leave play" $ do
      withAttribute token
        $ withLocation (Active, Play)
        $ addArtifact "Treasure"

      move (Active, Play) (Active, Graveyard) "Treasure"
      validateRemoved "Treasure"
  , prove "does not allow spell copies to move off the stack" $ do
      withAttribute copy
        $ withLocation (Active, Stack)
        $ addInstant "Shock"

      move (Active, Stack) (Active, Graveyard) "Shock"
      validateRemoved "Shock"
  , prove "exiles jumpstart'ed cards" $ do
      withLocation (Active, Stack) $ addInstant "Chemister's Insight"
      gainAttribute exileWhenLeaveStack "Chemister's Insight"

      move (Active, Stack) (Active, Graveyard) "Chemister's Insight"

      validate "Chemister's Insight" $
           matchLocation (Active, Exile)
        <> missingAttribute exileWhenLeaveStack
  , prove "adds summoned attribute when moving to play" $ do
      withLocation (Active, Hand) $ addLand "Forest"
      move (Active, Hand) (Active, Play) "Forest"

      validate "Forest" $ matchAttribute summoned
  , prove "removes gained attribute when leaving play" $ do
      withLocation (Active, Play) $ addLand "Forest"

      gainAttribute "bogus" "Forest"

      move (Active, Play) (Active, Hand) "Forest"

      validate "Forest" $ missingAttribute "bogus"
  , prove "adds +1/+1 to when undying" $ do
      withLocation (Active, Play)
        $ withAttribute undying
        $ addCreature (1, 1) "Zombie"

      moveTo Graveyard "Zombie"

      validate "Zombie" $
        matchLocation (Active, Play) <> matchPlusOneCounters 1
  , prove "moves to graveyard when undying and has +1/+1 counters" $ do
      withLocation (Active, Play)
        $ withAttribute undying
        $ withPlusOneCounters 3
        $ addCreature (1, 1) "Zombie"

      moveTo Graveyard "Zombie"

      validate "Zombie" $
        matchLocation (Active, Graveyard) <> matchPlusOneCounters 0
  , prove "removes damage when leaving play" $ do
      withLocation (Active, Play) $ addCreature (1, 1) "Zombie"
      modifyCard "Zombie" cardDamage (const 1)

      moveTo Graveyard "Zombie"

      validate "Zombie" $ matchDamage 0
  , prove "moves card to graveyard of same owner" $ do
      withLocation (Opponent, Hand) $ addLand "Forest"
      moveTo Graveyard "Forest"
      validate "Forest" $ matchLocation (Opponent, Graveyard)
  , refute
      "cannot move to stack"
      "cannot move directly to stack" $ do
        withLocation (Active, Hand) $ addInstant "Shock"

        move (Active, Hand) (Active, Stack) "Shock"
  , refute
      "requires card exists"
      "Card does not exist: Forest" $ do
        move (Active, Hand) (Active, Exile) "Forest"
  , refute
      "cannot move to same location"
      "cannot move to same location" $ do
        withLocation (Active, Hand) $ addInstant "Shock"

        move (Active, Hand) (Active, Hand) "Shock"
  ]
