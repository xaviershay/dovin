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
        "Zombie does not match requirements: in zone Hand" $ do
          withLocation (Active, Play) $ addCreature (1, 1) "Zombie"
          cast "" "Zombie"
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
    , prove "removes multiple digits for colorless mana from pool" $ do
        addMana "9"
        addMana "9"
        spendMana "15"
        validateBoardEquals (manaPoolFor Active) "XXX"
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
    , prove "adds mana with multiple digits to pool" $ do
        addMana "10"
        validateBoardEquals (manaPool . at Active . _Just) (replicate 10 'X')
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
    , refute
        "prevents defenders from attacking"
        "not has attribute defender" $ do
          withLocation (Active, Play) $ do
            withAttributes [defender] $ addCreature (1, 1) "Wall"

          attackWith ["Wall"]
    , prove "allows hasty creatures to attack" $ do
        withLocation (Active, Play) $ do
          withAttributes [haste, summoned] $ addCreature (1, 1) "Bat"

        attackWith ["Bat"]
        validate "Bat" $ matchAttributes [attacking, tapped]
    ]
  ]

