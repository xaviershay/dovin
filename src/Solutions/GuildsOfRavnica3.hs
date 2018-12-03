module Solutions.GuildsOfRavnica3 where

import Control.Monad

import Dovin

solution :: GameMonad ()
solution = do
  let sacrificeToNecrolisk =
        \name -> do
          activate "1" "Undercity Necrolisk"
          validate name $
               matchInPlay
            <> matchController Active
            <> matchAttribute "creature"
          sacrifice name
          gainAttribute "Undercity Necrolisk" "menace"
          modifyStrength "Undercity Necrolisk" (1, 1)
          whenMatch "Pitiless Plunderer" matchInPlay $ do
            trigger "Pitiless Plunderer"
            addCard "Treasure" (Active, Play) ["artifact", "token"]

  step "Initial state" $ do
    setLife Opponent 9

    addCreature "Hunted Witness" (1, 1) (Active, Hand) ["lifelink"]
    addCreature "Silverclad Ferocidons" (8, 5) (Active, Hand) []
    addCard "Justice Strike" (Active, Hand) ["instant"]

    addCreature "Roc Charger" (9, 3) (Active, Play) ["menace"]
    addCard "Desecrated Tomb" (Active, Play) ["artifact"]
    addCreature "Undercity Necrolisk" (3, 3) (Active, Play) []
    addCreature "Pitiless Plunderer" (1, 4) (Active, Play) []
    addCreature "Oathsworn Vampire" (2, 2) (Active, Play) []

    addCards 4 "Boros Guildgate" (Active, Play) ["land"]
    addCards 4 "Gateway Plaza" (Active, Play) ["land"]

  step "Cast Hunted Witness and sac it" $ do
    tapForMana "Boros Guildgate 1" "W"
    cast "W" "Hunted Witness"
    resolve "Hunted Witness"

    tapForMana "Boros Guildgate 2" "W"
    sacrificeToNecrolisk "Hunted Witness"
    addToken "Soldier" (1, 1) (Active, Play) ["lifelink"]

  step "Sac Oathsworn Vampire" $ do
    tapForMana "Treasure" "1"
    sacrifice "Treasure"
    sacrificeToNecrolisk "Oathsworn Vampire"

  step "Justice Strike soldier to trigger life gain" $ do
    tapForMana "Treasure" "W"
    sacrifice "Treasure"
    tapForMana "Boros Guildgate 3" "R"

    cast "RW" "Justice Strike"
    resolve "Justice Strike"
    target "Soldier"
    fight "Soldier" "Soldier"

  step "Cast Oathsworn Vampire from Graveyard, triggering Desecrated Tomb" $ do
    tapForMana "Boros Guildgate 4" "R"
    tapForMana "Gateway Plaza 1" "B"
    castFromLocation (Active, Graveyard) "1B" "Oathsworn Vampire"
    resolve "Oathsworn Vampire"

    trigger "Desecrated Tomb"
    addToken "Bat" (1, 1) (Active, Play) ["flying"]

  step "Sac vampire, bat, and plunderer" $ do
    tapForMana "Gateway Plaza 2" "B"
    sacrificeToNecrolisk "Oathsworn Vampire"

    tapForMana "Treasure" "1"
    sacrifice "Treasure"
    sacrificeToNecrolisk "Bat"

    tapForMana "Treasure" "1"
    sacrifice "Treasure"
    sacrificeToNecrolisk "Pitiless Plunderer"

  step "Attack with Roc and Necrolisk" $ do
    validate "Roc Charger" $ matchAttribute "menace"
    validate "Undercity Necrolisk" $ matchAttribute "menace"

    attackWith ["Roc Charger", "Undercity Necrolisk"]

  fork $
    [ step "Roc is blocked" $ do
        gainAttribute "Roc Charger" "blocked"

        tapForMana "Gateway Plaza 3" "B"
        sacrificeToNecrolisk "Roc Charger"

        damagePlayer "Undercity Necrolisk"

        validateLife Opponent 0
    , step "Roc is blocked" $ do
        gainAttribute "Undercity Necrolisk" "blocked"

        damagePlayer "Roc Charger"

        validateLife Opponent 0
    ]
