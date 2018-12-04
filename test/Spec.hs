{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Spec where

import Test.Tasty
import Test.Tasty.HUnit

import Dovin

import Control.Monad
import Control.Lens
import Control.Monad.Except

prove name m = testCase name $
  case runMonad emptyBoard m of
    (Left msg, _, _) -> assertFailure msg
    (Right (), _, _) -> return mempty

refute name expectedFailure m = testCase name $
  case runMonad emptyBoard m of
    (Left msg, _, _) -> expectedFailure @=? msg
    (Right (), _, _) -> assertFailure "proof was not refuted"

validateBoardEquals lens expected = do
  x <- use lens

  unless (x == expected) $
    throwError ("want: " <> show expected <> ", got: " <> show x)

test_Test = testGroup "Actions"
  [ testGroup "cast"
    [ prove "casts from hand to stack" $ do
        addCard "Zombie" (Active, Hand) []
        addMana "B"
        castFromLocation (Active, Hand) "B" "Zombie"
        validate "Zombie" $ matchLocation (Active, Stack)
        validateBoardEquals manaPool mempty

    , refute
        "requires card to be in hand"
        "Zombie does not match requirements: in location (Active,Hand)" $ do
          addCard "Zombie" (Active, Graveyard) []
          cast "" "Zombie"
    ]
  , testGroup "castFromLocation"
    [ prove "places card on top of stack, spending mana" $ do
        addCard "Zombie" (Active, Graveyard) []
        addMana "B"
        castFromLocation (Active, Graveyard) "B" "Zombie"
        validate "Zombie" $ matchLocation (Active, Stack)
        validateBoardEquals manaPool mempty

    , refute
        "fails if mana not available"
        "Mana pool () does not contain (B)" $ do
          addCard "Zombie" (Active, Hand) []
          castFromLocation (Active, Hand) "B" "Zombie"

    , prove "increases storm count if instant" $ do
        addCard "Shock" (Active, Hand) ["instant"]
        castFromLocation (Active, Hand) "" "Shock"
        validateBoardEquals (counters . at "storm" . non 0) 1

    , prove "increases storm count if sorcery" $ do
        addCard "Lava Spike" (Active, Hand) ["sorcery"]
        castFromLocation (Active, Hand) "" "Lava Spike"

    , prove "does not increase storm count otherwise" $ do
        addCard "Mox Amber" (Active, Hand) []
        castFromLocation (Active, Hand) "" "Mox Amber"
        validateBoardEquals (counters . at "storm" . non 0) 0
    ]
  , testGroup "move"
    [ prove "moves card from one location to another" $ do
        addCard "Zombie" (Active, Hand) []
        move (Active, Hand) (Active, Exile) "Zombie"

        validate "Zombie" $ matchLocation (Active, Exile)
    , refute
        "fails if card does not exist at location"
        "Card does not exist: Zombie" $ do
          move (Active, Hand) (Active, Exile) "Zombie"
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
        "fails if no mana"
        "Mana pool () does not contain (X)" $ do
          spendMana "1"
    , refute
        "fails if insufficent mana"
        "Mana pool () does not contain (X)" $ do
          addMana "R"
          spendMana "2"
    , refute
        "fails if wrong color mana"
        "Mana pool (R) does not contain (W)" $ do
          addMana "R"
          spendMana "W"
    ]
  ]
