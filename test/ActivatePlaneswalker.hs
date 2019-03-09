module ActivatePlaneswalker where

import TestPrelude.V2

test_ActivatePlaneswalker = testGroup "activatePlaneswalker"
  [ prove "adds activated card to stack" $ do
      withLocation Play $ addPlaneswalker 4 "Karn, Scion of Urza"
      activatePlaneswalker "Reveal cards" (-1) "Karn, Scion of Urza"

      validate
        (matchLocation (Active, Stack) <> matchAttribute activated)
        "Reveal cards"
      validate (matchLoyalty 3) "Karn, Scion of Urza"

      resolve "Reveal cards"
  , refute
      "requires card in play"
      "in play" $ do
        withLocation Graveyard $ addPlaneswalker 4 "Karn, Scion of Urza"
        activatePlaneswalker "Reveal cards" (-1) "Karn, Scion of Urza"
  , refute
      "requires card controlled by actor"
      "has controller Opponent" $ do
        withLocation Play $ addPlaneswalker 4 "Karn, Scion of Urza"
        as Opponent $
          activatePlaneswalker "Reveal cards" (-1) "Karn, Scion of Urza"
  , refute
      "requires sufficient loyalty"
      "does not have enough loyalty" $ do
        withLocation Play $ addPlaneswalker 1 "Karn, Scion of Urza"
        activatePlaneswalker "Create construct" (-2) "Karn, Scion of Urza"
  ]
