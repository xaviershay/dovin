module Move where

import TestPrelude.V3

test_Move = testGroup "move/moveTo"
  [ prove "moves card from one location to another" $ do
      withLocation Hand $ addLand "Forest"
      move (Active, Hand) (Active, Exile) "Forest"

      validate (matchZone Exile) "Forest"
  , prove "exiles jumpstart'ed cards" $ do
      withLocation Stack $ addInstant "Chemister's Insight"
      gainAttribute exileWhenLeaveStack "Chemister's Insight"

      move (Active, Stack) (Active, Graveyard) "Chemister's Insight"

      validate (
           matchZone Exile
        <> missingAttribute exileWhenLeaveStack
        ) "Chemister's Insight"
  , prove "adds summoned attribute when moving to play" $ do
      withLocation Hand $ addLand "Forest"
      move (Active, Hand) (Active, Play) "Forest"

      validate (matchAttribute summoned) "Forest"
  , prove "removes gained attribute when leaving play" $ do
      withLocation Play $ addLand "Forest"

      gainAttribute "bogus" "Forest"

      move (Active, Play) (Active, Hand) "Forest"

      validate (missingAttribute "bogus") "Forest"
  , prove "adds +1/+1 to when undying" $ do
      withLocation Play
        $ withAttribute undying
        $ addCreature (1, 1) "Zombie"

      moveTo Graveyard "Zombie"

      validate (matchZone Play <> matchPlusOneCounters 1) "Zombie"
  , prove "moves to graveyard when undying and has +1/+1 counters" $ do
      withLocation Play
        $ withAttribute undying
        $ withPlusOneCounters 3
        $ addCreature (1, 1) "Zombie"

      moveTo Graveyard "Zombie"

      validate (matchZone Graveyard <> matchPlusOneCounters 0) "Zombie"
  , prove "removes damage when leaving play" $ do
      withLocation Play $ addCreature (1, 1) "Zombie"
      modifyCardDeprecated "Zombie" cardDamage (const 1)

      moveTo Graveyard "Zombie"

      validate (matchDamage 0) "Zombie"
  , prove "moves card to graveyard of same owner" $ do
      as Opponent $ do
        withLocation Hand $ addLand "Forest"
        moveTo Graveyard "Forest"
        validate (matchController Opponent <> matchZone Graveyard) "Forest"
  , prove "can change controller of token" $ do
      withLocation Play
        $ withAttribute token
        $ addArtifact "Treasure"

      move (Active, Play) (Opponent, Play) "Treasure"
      validate (matchZone Play) "Treasure"
  , refute
      "cannot move to stack"
      "cannot move directly to stack" $ do
        withLocation Hand $ addInstant "Shock"

        move (Active, Hand) (Active, Stack) "Shock"
  , refute
      "cannot move to deck"
      "cannot move directly to deck" $ do
        withLocation Graveyard $ addInstant "Shock"

        move (Active, Graveyard) (Active, Deck) "Shock"
  , refute
      "requires card exists"
      "Card does not exist: Forest" $ do
        move (Active, Hand) (Active, Exile) "Forest"
  , refute
      "cannot move to same location"
      "cannot move to same location" $ do
        withLocation Hand $ addInstant "Shock"

        move (Active, Hand) (Active, Hand) "Shock"
  , refute
      "cannot move token from non-play location"
      "cannot move token from non-play location" $ do
        withLocation Graveyard
          $ withAttribute token
          $ addArtifact "Treasure"

        move (Active, Graveyard) (Active, Play) "Treasure"
  , refute
      "cannot move copy from non-stack location"
      "cannot move copy from non-stack location" $ do
        withLocation Hand $ addInstant "Shock"

        cast "" "Shock"
        copySpell "Shock 1" "Shock" >> resolveTop

        move (Active, Graveyard) (Active, Play) "Shock 1"
  ]
