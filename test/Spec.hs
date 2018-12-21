{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Spec where

import Test.Tasty
import Test.Tasty.HUnit

import Dovin
import Dovin.Monad

import Control.Monad
import Control.Lens
import Control.Monad.Except
import Data.List (isInfixOf)

prove name m = testCase name $
  case runMonad emptyBoard m of
    (Left msg, _, _) -> assertFailure msg
    (Right (), _, _) -> return mempty

refute name expectedFailure m = testCase name $
  case runMonad emptyBoard m of
    (Left msg, _, _) -> assertBool ("expected: " <> expectedFailure <> "\n but got: " <> msg) $ expectedFailure `isInfixOf` msg
    (Right (), _, _) -> assertFailure "proof was not refuted"

validateBoardEquals lens expected = do
  x <- use lens

  unless (x == expected) $
    throwError ("want: " <> show expected <> ", got: " <> show x)

test_Test = testGroup "Actions"
  [ testGroup "cast"
    [ prove "casts from hand to stack" $ do
        withLocation (Active, Hand) $ addCreature (1, 1) "Zombie"
        addMana "B"
        castFromLocation (Active, Hand) "B" "Zombie"
        validate "Zombie" $ matchLocation (Active, Stack)
        validateBoardEquals manaPool mempty

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
        validateBoardEquals manaPool mempty

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
  , testGroup "move"
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
    , prove "does not allow copies to leave play" $ do
        withAttribute copy
          $ withLocation (Active, Stack)
          $ addInstant "Shock"

        move (Active, Stack) (Active, Graveyard) "Shock"
        validateRemoved "Shock"
    , prove "exiles jumpstart'ed cards" $ do
        withAttribute exileWhenLeaveStack
          $ withLocation (Active, Stack)
          $ addInstant "Chemister's Insight"

        move (Active, Stack) (Active, Graveyard) "Chemister's Insight"
        validate "Chemister's Insight" $
             matchLocation (Active, Exile)
          <> invert (matchAttribute exileWhenLeaveStack)
    , prove "adds summonded attribute when moving to play" $ do
        withLocation (Active, Hand) $ addLand "Forest"
        move (Active, Hand) (Active, Play) "Forest"

        validate "Forest" $ matchAttribute summoned
    , prove "removes summonded attribute when leaving play" $ do
        withAttribute summoned
          $ withLocation (Active, Play)
          $ addLand "Forest"

        move (Active, Play) (Active, Hand) "Forest"

        validate "Forest" $ invert (matchAttribute summoned)
    , refute
        "cannot move to stack"
        "cannot move directly to stack" $ do
          withLocation (Active, Hand) $ addInstant "Shock"

          move (Active, Hand) (Active, Stack) "Shock"
    , refute
        "requires card exists"
        "Card does not exist: Forest" $ do
          move (Active, Hand) (Active, Exile) "Forest"
    ]
  , testGroup "moveToGraveyard"
    [ prove "moves card to graveyard" $ do
        withLocation (Opponent, Hand) $ addLand "Forest"
        moveToGraveyard "Forest"
        validate "Forest" $ matchLocation (Opponent, Graveyard)
    ]
  , testGroup "spendMana"
    [ prove "removes colored mana from pool" $ do
        addMana "BBRRRWW"
        spendMana "RB"
        validateBoardEquals manaPool "BRRWW"
    , prove "removes colorless mana from pool" $ do
        addMana "RWB"
        spendMana "3"
        validateBoardEquals manaPool mempty
    , prove "removes colored mana before colorless" $ do
        addMana "RWB"
        spendMana "1R"
        validateBoardEquals manaPool "W"
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
        validateBoardEquals manaPool "GRXX"
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
        "in location" $ do
          withLocation (Active, Graveyard) $ addLand "Forest"
          tap "Forest"
    ]
  , testGroup "tapForMana"
    [ prove "taps card and adds mana to pool" $ do
        withLocation (Active, Play) $ addLand "Forest"
        tapForMana "G" "Forest"

        validate "Forest" $ matchAttribute tapped
        validateBoardEquals manaPool "G"
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
  , testGroup "resolveTop"
    [ prove "resolves top card of stack" $ do
        withLocation (Active, Hand) $ addInstant "Shock"
        cast "" "Shock" >> resolveTop

        validate "Shock" $ matchLocation (Active, Graveyard)
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

  ]
