module Solutions.Dominaria5 where

import Dovin
import Dovin.Prelude

solution :: GameMonad ()
solution = do
  step "Initial state" $ do
    setLife Opponent 8
    withLocation (Active, Hand) $ do
      addPlaneswalker 5 "Karn, Scion of Urza 2"

    withLocation (Active, Play) $ do
      addPlaneswalker 2 "Karn, Scion of Urza 1"
      addCreature (3, 3) "Weldfast Wingsmith"
      withAttribute doublestrike $
        addCreature (2, 2) "Storm Fleet Swashbuckler"
      addCreature (1, 3) "Reckless Fireweaver"

      addLands 3 "Sulfur Falls"
      addLands 3 "Canyon Slough"

    withLocation (Active, Exile) $ do
      addInstant "Fatal Push"
      addSorcery "Mutiny"

    withLocation (Opponent, Play) $ do
      addCreature (2, 3) "Aerial Responder 1"
      addCreature (2, 3) "Aerial Responder 2"
      addCreature (5, 5) "Bonded Horncrest"

  step "Activate Karn to return Mutiny" $ do
    activatePlaneswalker (-1) "Karn, Scion of Urza 1"
    moveTo Hand "Mutiny"

  step "Cast Mutiny, using Horncrest to destroy a Responder" $ do
    tapForMana "R" "Sulfur Falls 1"
    cast "R" "Mutiny" >> resolveTop
    target "Bonded Horncrest"
    target "Aerial Responder 1"
    damage (view cardPower) (targetCard "Aerial Responder 1") "Bonded Horncrest"

  step "Cast Karn, using legend rule to remove the existing one" $ do
    tapForMana "U" "Sulfur Falls 2"
    tapForMana "U" "Sulfur Falls 3"
    tapForMana "B" "Canyon Slough 1"
    tapForMana "B" "Canyon Slough 2"
    cast "4" "Karn, Scion of Urza 2" >> resolveTop
    moveTo Graveyard "Karn, Scion of Urza 1"

  step "Activate Karn to return Fatal Push" $ do
    activatePlaneswalker (-1) "Karn, Scion of Urza 2"
    moveTo Hand "Fatal Push"

  step "Cast Fatal Push destroying remaining Responder with Revolt from first Karn" $ do
    tapForMana "B" "Canyon Slough 3"
    cast "B" "Fatal Push" >> resolveTop
    target "Aerial Responder 2"
    destroy "Aerial Responder 2"

  step "Attack with all for lethal, since Horncrest can't block alone" $ do
    validate "Aerial Responder 1" $ invert matchInPlay
    validate "Aerial Responder 2" $ invert matchInPlay

    attackWith
      [ "Weldfast Wingsmith"
      , "Storm Fleet Swashbuckler"
      , "Reckless Fireweaver"
      ]

    forCards
      (matchAttributes [attacking, doublestrike])
      (combatDamage [])

    forCards
      (matchAttributes [attacking])
      (combatDamage [])

    validateLife Opponent 0

attributes = attributeFormatter $ do
  attribute "life" $ countLife Opponent
  attribute "mana" $
    (+) <$> countCards
              ( matchAttribute "land"
              <> missingAttribute "tapped"
              <> matchController Active
              )
        <*> countManaPool
formatter 1 = boardFormatter
formatter 3 = attributes
  <> cardFormatter
       "remaining creatures"
       (matchLocation (Opponent, Play) <> matchAttribute creature)
formatter 6 = attributes
  <> cardFormatter
       "remaining creatures"
       (matchLocation (Opponent, Play) <> matchAttribute creature)
formatter 7 = attributes
  <> cardFormatter
      "unblocked creatures"
      (matchLocation (Active, Play)
      <> matchAttribute creature
      <> invert (matchAttribute "blocked")
      )
formatter _ = attributes
