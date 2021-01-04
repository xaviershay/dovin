module Damage where

import TestPrelude

test_Damage = testGroup "damage"
  [ prove "adds damage to a creature" $ do
      withLocation (Active, Play) $ addCreature (4, 4) "Angel"
      addInstant "Shock"

      damage (const 2) (targetCard "Angel") "Shock"
      validate "Angel" $ matchInPlay <> matchDamage 2
      validateLife Active 0
      validateLife Opponent 0
  , prove "removes loyalty counters from a planeswalker" $ do
      withLocation (Active, Play) $ addPlaneswalker 3 "Jeff"
      addInstant "Shock"
      damage (const 2) (targetCard "Jeff") "Shock"
      validate "Jeff" $ matchInPlay <> matchLoyalty 1
  , prove "adds damage computed from function" $ do
      withLocation (Active, Play) $ addCreature (1, 4) "Angel"

      damage ((* 2) . view cardPower) (targetCard "Angel") "Angel"
      validate "Angel" $ matchDamage 2
  , prove "does not destroy creature since SBAs aren't run" $ do
      withLocation (Active, Play) $ addCreature (2, 2) "Angel"
      addInstant "Shock"

      damage (const 2) (targetCard "Angel") "Shock"
      validate "Angel" $ matchInPlay <> matchDamage 2
  , prove "cannot destroy indestructible creatures" $ do
      withLocation (Active, Play)
        $ withAttribute indestructible
        $ addCreature (2, 2) "Angel"
      addInstant "Shock"

      damage (const 2) (targetCard "Angel") "Shock"
      validate "Angel" $ matchInPlay <> matchDamage 2
  , prove "target gains deathtouched when source has deathtouch" $ do
      withLocation (Active, Play)
        $ addCreature (2, 2) "Angel"
      withLocation (Active, Play)
        $ withAttribute deathtouch
        $ addCreature (1, 4) "Spider"

      damage (const 1) (targetCard "Angel") "Spider"
      validate "Angel" $ matchAttribute deathtouched
  , prove "target does not gain deathtouched when no damage done" $ do
      withLocation (Active, Play)
        $ addCreature (2, 2) "Angel"
      withLocation (Active, Play)
        $ withAttribute deathtouch
        $ addCreature (1, 4) "Spider"

      damage (const 0) (targetCard "Angel") "Spider"
      validate "Angel" $ missingAttribute deathtouched
  , prove "gains life when source has lifelink" $ do
      withLocation (Opponent, Play)
        $ withAttribute lifelink
        $ addCreature (2, 2) "Angel"

      damage (const 2) (targetCard "Angel") "Angel"
      validateLife Opponent 2
  , refute
      "requires target to be in play"
      "in play" $ do
        withLocation (Active, Graveyard) $ addCreature (4, 4) "Angel"
        addInstant "Shock"

        damage (const 2) (targetCard "Angel") "Shock"
  , refute
      "requires target to be a creature"
      "has attribute creature" $ do
        withLocation (Active, Play) $ addArtifact "Mox"
        addInstant "Shock"

        damage (const 2) (targetCard "Mox") "Shock"
  , refute
      "requires damage to be >= 0"
      "damage must be positive, was -1" $ do
        withLocation (Active, Play) $ addCreature (4, 4) "Angel"

        damage (const (-1)) (targetCard "Angel") "Angel"
  , prove "damages player" $ do
      addInstant "Shock"
      damage (const 2) (targetPlayer Opponent) "Shock"

      validateLife Opponent (-2)
  ]

test_CombatDamage = testGroup "combatDamage"
  [ prove "damages opponent when no blockers" $ do
      withLocation (Active, Play) $ addCreature (4, 4) "Angel"
      attackWith ["Angel"]
      combatDamage [] "Angel"
      validateLife Opponent (-4)
  , prove "damages blocker instead of player" $ do
      withLocation (Opponent, Play) $ addCreature (1, 1) "Spirit"
      withLocation (Active, Play) $ addCreature (4, 4) "Angel"
      attackWith ["Angel"]
      combatDamage ["Spirit"] "Angel"
      validateLife Opponent 0
      validate "Spirit" $ matchDamage 4
  , prove "only deals as much damage to blockers as available power" $ do
      withLocation (Opponent, Play) $ addCreature (1, 1) "Spirit"
      withLocation (Opponent, Play) $ addCreature (6, 6) "Dragon"
      withLocation (Active, Play) $ addCreature (4, 4) "Angel"
      attackWith ["Angel"]
      combatDamage ["Spirit", "Dragon"] "Angel"
      validateLife Opponent 0
      validate "Spirit" $ matchDamage 1
      validate "Dragon" $ matchDamage 3
      validate "Angel" $ matchDamage 7
  , prove "deals excess damage to player with trample" $ do
      withLocation (Opponent, Play) $ addCreature (1, 1) "Spirit"
      withLocation (Active, Play)
        $ withAttributes [lifelink, trample]
        $ addCreature (4, 4) "Angel"
      attackWith ["Angel"]
      combatDamage ["Spirit"] "Angel"
      validateLife Opponent (-3)
      validateLife Active 4
  , prove "deals all damage, primarily for lifelink purposes" $ do
      withLocation (Opponent, Play) $ addCreature (1, 1) "Spirit"
      withLocation (Active, Play)
        $ withAttributes [lifelink]
        $ addCreature (4, 4) "Angel"
      attackWith ["Angel"]
      combatDamage ["Spirit"] "Angel"
      validateLife Active 4
  , prove "deals damage to opposing player of actor" $ do
      as Opponent $ do
        withLocation (Opponent, Play) $ addCreature (4, 4) "Angel"
        attackWith ["Angel"]
        combatDamage [] "Angel"
        validateLife Active (-4)
  , refute
      "requires attacking creature"
      "has attribute attacking" $ do
        withLocation (Active, Play) $ addCreature (4, 4) "Angel"
        combatDamage [] "Angel"
  , refute
      "requires in play creature"
      "in play" $ do
        withLocation (Active, Graveyard) $ addCreature (4, 4) "Angel"
        gainAttribute attacking "Angel"
        combatDamage [] "Angel"
  , refute
      "requires in play blocker"
      "in play" $ do
        withLocation (Active, Play) $ addCreature (4, 4) "Angel"
        withLocation (Opponent, Graveyard) $ addCreature (1, 1) "Spirit"
        gainAttribute attacking "Angel"
        combatDamage ["Spirit"] "Angel"
  , refute
      "requires control of attacking creature"
      "has controller Opponent" $ do
        withLocation (Active, Play) $ addCreature (4, 4) "Angel"
        attackWith ["Angel"]
        as Opponent $ combatDamage [] "Angel"
  ]

test_CombatDamageTo = testGroup "combatDamageTo"
  [ prove "damages opponent when no blockers" $ do
      withLocation (Active, Play) $ addCreature (4, 4) "Angel"
      attackWith ["Angel"]
      combatDamageTo (TargetPlayer $ OpponentN 1) [] "Angel"
      validateLife Opponent 0
      validateLife (OpponentN 1) (-4)
  ]
