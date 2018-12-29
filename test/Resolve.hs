module Resolve where

import TestPrelude.V2

test_Resolve = testGroup "resolve"
  [ testGroup "resolveTop"
    [ prove "resolves top spell from stack" $ do
        withLocation Hand $ addInstant "Shock"
        cast "" "Shock" >> resolveTop

        validate (matchLocation (Active, Graveyard)) "Shock"
        validateBoardEquals stack mempty
    , prove "resolves top permanent of stack" $ do
        withLocation Hand $ addArtifact "Mox Opal"
        cast "" "Mox Opal" >> resolveTop

        validate (matchLocation (Active, Play)) "Mox Opal"
        validateBoardEquals stack mempty
    , prove "resolves top trigger of stack" $ do
        withLocation Play $ addArtifact "Phrexian Altar"
        trigger "Trigger" "Phrexian Altar" >> resolveTop

        validateBoardEquals stack mempty
        validateRemoved "Trigger"
    , prove "resolves top activation of stack" $ do
        withLocation Play $ addArtifact "Phrexian Altar"
        activate "Mana Ability" "" "Phrexian Altar" >> resolveTop

        validateBoardEquals stack mempty
        validateRemoved "Mana Ability"
    , refute
        "requires non-empty stack"
        "stack is empty" $ do
          resolveTop
    ]
  , testGroup "resolve"
    [ prove "resolves top card of stack" $ do
        withLocation Hand $ addInstant "Shock"
        cast "" "Shock" >> resolve "Shock"

        validate (matchLocation (Active, Graveyard)) "Shock"
        validateBoardEquals stack mempty
    , refute
        "requires top card to match provided"
        "unexpected top of stack: expected Shock 1, got Shock 2" $ do
          withLocation Hand $ addInstant "Shock 1"
          withLocation Hand $ addInstant "Shock 2"
          cast "" "Shock 1"
          cast "" "Shock 2"
          resolve "Shock 1"
    , refute
        "requires non-empty stack"
        "stack is empty" $ do
          withLocation Hand $ addInstant "Shock"
          resolve "Shock"
    ]
  ]

