module Move where

import TestPrelude.V4

test_Move = testGroup "moveTo"
  [ prove "moves card from one location to another" $ do
      withZone Hand $ addLand "Forest"
      moveTo Exile "Forest"
      validate (matchZone Exile) "Forest"
  , prove "exiles jumpstart'ed cards" $ do
      withZone Stack $ addInstant "Chemister's Insight"
      gainAttribute exileWhenLeaveStack "Chemister's Insight"

      moveTo Graveyard "Chemister's Insight"

      validate (
           matchZone Exile
        <> missingAttribute exileWhenLeaveStack
        ) "Chemister's Insight"
  , prove "adds summoned attribute when moving to play" $ do
      withZone Hand $ addLand "Forest"
      moveTo Play "Forest"
      validate (matchAttribute summoned) "Forest"
  , prove "removes gained attribute when leaving play" $ do
      withZone Play $ addLand "Forest"
      gainAttribute "bogus" "Forest"
      moveTo Hand "Forest"
      validate (missingAttribute "bogus") "Forest"
  , prove "adds +1/+1 to when undying" $ do
      withZone Play
        $ withAttribute undying
        $ addCreature (1, 1) "Zombie"

      moveTo Graveyard "Zombie"
      validate (matchZone Play <> matchPlusOneCounters 1) "Zombie"
  , prove "moves to graveyard when undying and has +1/+1 counters" $ do
      withZone Play
        $ withAttribute undying
        $ withPlusOneCounters 3
        $ addCreature (1, 1) "Zombie"

      moveTo Graveyard "Zombie"

      validate (matchZone Graveyard <> matchPlusOneCounters 0) "Zombie"
  , prove "removes damage when leaving play" $ do
      withZone Play $ addCreature (1, 1) "Zombie"
      modifyCardDeprecated "Zombie" cardDamage (const 1)

      moveTo Graveyard "Zombie"

      validate (matchDamage 0) "Zombie"
  , prove "moves card to graveyard of same owner" $ do
      as Opponent $ do
        withZone Hand $ addLand "Forest"
        moveTo Graveyard "Forest"
        validate (matchController Opponent <> matchZone Graveyard) "Forest"
  -- TODO: This is no longer possible with current API. Needs to be possible
  -- but need to think about whether an extension of moveTo or something else.
  --
  --, prove "can change controller of token" $ do
  --    withZone Play
  --      $ withAttribute token
  --      $ addArtifact "Treasure"

  --    as Opponent $
  --      moveTo Play "Treasure"

  --    validate (matchZone Play <> matchController Opponent) "Treasure"
  , prove "moving from deck removes from ordering" $ do
      withZone Deck $ addLand "Forest"
      withZone Deck $ addLand "Mountain"
      moveTo Hand "Forest"

      xs <- use $ deckFor Active

      case xs of
        ["Mountain"] -> return mempty
        xs' -> throwError $ "Deck ordering was not correct: " <> (show xs')
  , refute
      "cannot move to stack"
      "cannot move directly to stack" $ do
        withZone Hand $ addInstant "Shock"

        moveTo Stack "Shock"
  , refute
      "cannot move to deck"
      "cannot move directly to deck" $ do
        withZone Graveyard $ addInstant "Shock"

        moveTo Deck "Shock"
  , refute
      "requires card exists"
      "Card does not exist: Forest" $ do
        moveTo Exile "Forest"
  , refute
      "cannot move to same location"
      "cannot move to same location" $ do
        withZone Hand $ addInstant "Shock"

        moveTo Hand "Shock"
  , refute
      "cannot move token from non-play location"
      "cannot move token from non-play location" $ do
        withZone Graveyard
          $ withAttribute token
          $ addArtifact "Treasure"

        moveTo Play "Treasure"
  , refute
      "cannot move copy from non-stack location"
      "cannot move copy from non-stack location" $ do
        withZone Hand $ addInstant "Shock"

        cast "" "Shock"
        copySpell "Shock 1" "Shock" >> resolveTop

        moveTo Play "Shock 1"
  ]
