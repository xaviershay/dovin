{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Spec where

import TestPrelude

test_Test = testGroup "Actions"
  [ testGroup "cast"
    [ prove "casts from hand to stack" $ do
        withLocation (Active, Hand) $ addCreature (1, 1) "Zombie"
        addMana "B"
        castFromLocation (Active, Hand) "B" "Zombie"
        validate "Zombie" $ matchLocation (Active, Stack)
        validateBoardEquals (manaPoolFor Active) mempty
    , prove "can cast as opponent" $ do
        as Opponent $ do
          withLocation (Opponent, Hand) $ addCreature (1, 1) "Zombie"
          addMana "B"
          castFromLocation (Opponent, Hand) "B" "Zombie"
          validate "Zombie" $ matchLocation (Opponent, Stack)
          validateBoardEquals (manaPoolFor Opponent) mempty
    , refute
        "requires card to be in hand"
        "Zombie does not match requirements: in location (Active,Hand)" $ do
          withLocation (Active, Play) $ addCreature (1, 1) "Zombie"
          cast "" "Zombie"
    ]
  , testGroup "castFromLocation"
    [ prove "places card on top of stack, spending mana" $ do
        withLocation (Active, Graveyard) $ addCreature (1, 1) "Zombie"
        addMana "B"
        castFromLocation (Active, Graveyard) "B" "Zombie"
        validate "Zombie" $ matchLocation (Active, Stack)
        validateBoardEquals (manaPoolFor Active) mempty

    , refute
        "requires mana be available"
        "Mana pool () does not contain (B)" $ do
          withLocation (Active, Hand) $ addCreature (1, 1) "Zombie"
          castFromLocation (Active, Hand) "B" "Zombie"

    , prove "can cast non-instant in second main" $ do
        withLocation (Active, Hand) $ addSorcery "Lava Spike"
        transitionTo SecondMain
        castFromLocation (Active, Hand) "" "Lava Spike"

    , refute
        "requires main phase for non-instant"
        "not in a main phase" $ do
          withLocation (Active, Hand) $ addSorcery "Lava Spike"
          transitionTo BeginCombat
          castFromLocation (Active, Hand) "" "Lava Spike"

    , refute
        "requires stack to be empty for non-instant"
        "stack is not empty" $ do
          withLocation (Active, Hand) $ addInstant "Shock"
          withLocation (Active, Hand) $ addSorcery "Lava Spike"

          castFromLocation (Active, Hand) "" "Shock"
          castFromLocation (Active, Hand) "" "Lava Spike"

    , prove "increases storm count if instant" $ do
        withLocation (Active, Hand) $ addInstant "Shock"
        castFromLocation (Active, Hand) "" "Shock"
        validateBoardEquals (counters . at "storm" . non 0) 1

    , prove "increases storm count if sorcery" $ do
        withLocation (Active, Hand) $ addSorcery "Lava Spike"
        castFromLocation (Active, Hand) "" "Lava Spike"

    , prove "does not increase storm count otherwise" $ do
        withLocation (Active, Hand) $ addArtifact "Mox Amber"
        castFromLocation (Active, Hand) "" "Mox Amber"
        validateBoardEquals (counters . at "storm" . non 0) 0
    ]
  , testGroup "spendMana"
    [ prove "removes colored mana from pool" $ do
        addMana "BBRRRWW"
        spendMana "RB"
        validateBoardEquals (manaPoolFor Active) "BRRWW"
    , prove "removes colorless mana from pool" $ do
        addMana "RWB"
        spendMana "3"
        validateBoardEquals (manaPoolFor Active) mempty
    , prove "removes colored mana before colorless" $ do
        addMana "RWB"
        spendMana "1R"
        validateBoardEquals (manaPoolFor Active) "W"
    , refute
        "requires sufficient mana (colorless)"
        "Mana pool () does not contain (X)" $ do
          spendMana "1"
    , refute
        "requires sufficient mana (colored)"
        "Mana pool () does not contain (X)" $ do
          addMana "R"
          spendMana "2"
    , refute
        "requires right color mana"
        "Mana pool (R) does not contain (W)" $ do
          addMana "R"
          spendMana "W"
    ]
  , testGroup "addMana"
    [ prove "adds mana to pool" $ do
        addMana "2RG"
        validateBoardEquals (manaPool . at Active . _Just) "GRXX"
    ]
  , testGroup "tap"
    [ prove "taps card in play" $ do
        withLocation (Active, Play) $ addLand "Forest"
        tap "Forest"

        validate "Forest" $ matchAttribute tapped
    , prove "taps card in opponent's play" $ do
        withLocation (Active, Play) $ addLand "Forest"
        tap "Forest"

        validate "Forest" $ matchAttribute tapped
    , refute
        "requires card exists"
        "Card does not exist: Forest" $ do
          tap "Forest"
    , refute
        "requires untapped"
        "not has attribute tapped" $ do
          withAttribute tapped $ withLocation (Active, Play) $ addLand "Forest"
          tap "Forest"
    , refute
        "requires in play"
        "in play" $ do
          withLocation (Active, Graveyard) $ addLand "Forest"
          tap "Forest"
    ]
  , testGroup "tapForMana"
    [ prove "taps card and adds mana to pool" $ do
        withLocation (Active, Play) $ addLand "Forest"
        tapForMana "G" "Forest"

        validate "Forest" $ matchAttribute tapped
        validateBoardEquals (manaPoolFor Active) "G"
    ]
  , testGroup "transitionTo"
    [ prove "moves to new state" $ do
        transitionTo BeginCombat
        validatePhase BeginCombat
    , prove "empties mana pool" $ do
        addMana "B"
        transitionTo BeginCombat
        validateBoardEquals manaPool mempty
    , refute
        "requires later phase"
        "FirstMain does not occur after BeginCombat" $ do
          transitionTo BeginCombat
          transitionTo FirstMain
    ]
  , testGroup "transitionToForced"
    [ prove "can transition backwards" $ do
        transitionTo BeginCombat
        transitionToForced FirstMain
        validatePhase FirstMain
    ]
  , testGroup "untap"
    [ prove "untaps card in play" $ do
        withLocation (Active, Play) $ withAttribute tapped $  addLand "Forest"
        untap "Forest"

        validate "Forest" $ missingAttribute tapped
    , prove "untaps card in opponent's play" $ do
        withLocation (Opponent, Play) $ withAttribute tapped $ addLand "Forest"
        untap "Forest"

        validate "Forest" $ missingAttribute tapped
    , refute
        "requires card exists"
        "Card does not exist: Forest" $ do
          untap "Forest"
    , refute
        "requires tapped"
        "has attribute tapped" $ do
          withLocation (Active, Play) $ addLand "Forest"
          untap "Forest"
    , refute
        "requires in play"
        "in play" $ do
          withLocation (Active, Graveyard) $ addLand "Forest"
          untap "Forest"
    ]
  , testGroup "resolveTop"
    [ prove "resolves top spell from stack" $ do
        withLocation (Active, Hand) $ addInstant "Shock"
        cast "" "Shock" >> resolveTop

        validate "Shock" $ matchLocation (Active, Graveyard)
        validateBoardEquals stack mempty
    , prove "resolves top permanent of stack" $ do
        withLocation (Active, Hand) $ addArtifact "Mox Opal"
        cast "" "Mox Opal" >> resolveTop

        validate "Mox Opal" $ matchLocation (Active, Play)
        validateBoardEquals stack mempty
    , refute
        "requires non-empty stack"
        "stack is empty" $ do
          resolveTop
    ]
  , testGroup "resolve"
    [ prove "resolves top card of stack" $ do
        withLocation (Active, Hand) $ addInstant "Shock"
        cast "" "Shock" >> resolve "Shock"

        validate "Shock" $ matchLocation (Active, Graveyard)
        validateBoardEquals stack mempty
    , refute
        "requires top card to match provided"
        "unexpected top of stack: expected Shock 1, got Shock 2" $ do
          withLocation (Active, Hand) $ addInstant "Shock 1"
          withLocation (Active, Hand) $ addInstant "Shock 2"
          cast "" "Shock 1"
          cast "" "Shock 2"
          resolve "Shock 1"
    , refute
        "requires non-empty stack"
        "stack is empty" $ do
          withLocation (Active, Hand) $ addInstant "Shock"
          resolve "Shock"
    ]

  , testGroup "splice"
    [ prove "splices on to an arcane spell" $ do
        withLocation (Active, Hand) $ do
          addInstant "Glacial Ray"
          withAttribute arcane $ addInstant "Lava Spike"

        addMana "RRR"
        cast "R" "Lava Spike"
        splice "Lava Spike" "1R" "Glacial Ray"
        resolveTop

        validate "Glacial Ray" $ matchLocation (Active, Hand)
        validateBoardEquals stack mempty
        validateBoardEquals (manaPoolFor Active) mempty
    , prove "can splice as opponent" $ do
        withLocation (Opponent, Hand) $ do
          addInstant "Glacial Ray"
          withAttribute arcane $ addInstant "Lava Spike"

        as Opponent $ do
          addMana "RRR"
          cast "R" "Lava Spike"
          splice "Lava Spike" "1R" "Glacial Ray"
          resolveTop

          validate "Glacial Ray" $ matchLocation (Opponent, Hand)
          validateBoardEquals stack mempty
          validateBoardEquals (manaPoolFor Opponent) mempty
    , refute
      "requires arcane"
      "has attribute arcane" $ do
        withLocation (Active, Hand) $ do
          addInstant "Glacial Ray"
          addInstant "Lava Spike"

        cast "" "Lava Spike"
        splice "Lava Spike" "" "Glacial Ray"
    , refute
      "requires spliced spell on stack"
      "Lava Spike not on stack" $ do
        withLocation (Active, Hand) $ do
          addInstant "Glacial Ray"
          withAttribute arcane $ addInstant "Lava Spike"

        splice "Lava Spike" "" "Glacial Ray"
    , refute
      "requires spell in hand"
      "Glacial Ray not in hand" $ do
        withLocation (Active, Graveyard) $
          addInstant "Glacial Ray"
        withLocation (Active, Hand) $
          withAttribute arcane $ addInstant "Lava Spike"

        cast "" "Lava Spike"
        splice "Lava Spike" "" "Glacial Ray"
    ]
  , testGroup "attackWith"
    [ prove "attacks with listed creatures" $ do
        withLocation (Active, Play) $ do
          addCreature (1, 1) "Bat 1"
          addCreature (1, 1) "Bat 2"
          addCreature (1, 1) "Bat 3"

        attackWith ["Bat 1", "Bat 2"]

        validatePhase DeclareAttackers
        validate "Bat 1" $ matchAttributes [attacking, tapped]
        validate "Bat 2" $ matchAttributes [attacking, tapped]
        validate "Bat 3" $ missingAttribute attacking
        validate "Bat 3" $ missingAttribute tapped
    , prove "does not tap vigilant attackers" $ do
        withLocation (Active, Play) $ do
          withAttribute vigilance $ addCreature (1, 1) "Bat"

        attackWith ["Bat"]
        validate "Bat" $ matchAttribute attacking
        validate "Bat" $ missingAttribute tapped
    , refute
        "prevents summoned creatures from attacking"
        "does not have summoning sickness" $ do
          withLocation (Active, Play) $ do
            withAttributes [summoned] $ addCreature (1, 1) "Bat"

          attackWith ["Bat"]
    , prove "allows hasty creatures to attack" $ do
        withLocation (Active, Play) $ do
          withAttributes [haste, summoned] $ addCreature (1, 1) "Bat"

        attackWith ["Bat"]
        validate "Bat" $ matchAttributes [attacking, tapped]
    ]
  ]

