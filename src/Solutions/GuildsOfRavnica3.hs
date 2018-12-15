module Solutions.GuildsOfRavnica3 where

import Control.Monad

import Dovin

solution :: GameMonad ()
solution = do
  let menace = "menace"
  let sacrificeToNecrolisk =
        \name -> do
          validatePhase FirstMain
          activate "1" "Undercity Necrolisk"
          validate name $
               matchInPlay
            <> matchController Active
            <> matchAttribute "creature"
          sacrifice name
          gainAttribute menace "Undercity Necrolisk"
          modifyStrength "Undercity Necrolisk" (1, 1)
          whenMatch "Pitiless Plunderer" matchInPlay $ do
            trigger "Pitiless Plunderer"
            withLocation (Active, Play)
              $ withAttribute token
              $ addArtifact "Treasure"

  step "Initial state" $ do
    setLife Opponent 9

    withLocation (Active, Hand) $ do
      withAttribute lifelink $ addCreature (1, 1) "Hunted Witness"
      addCreature (8, 5) "Silverclad Ferocidons"
      addInstant "Justice Strike"

    withLocation (Active, Play) $ do
      withAttribute menace $ addCreature (9, 3) "Roc Charger"
      addArtifact "Desecrated Tomb"
      addCreature (3, 3) "Undercity Necrolisk"
      addCreature (1, 4) "Pitiless Plunderer"
      addCreature (2, 2) "Oathsworn Vampire"
      forM_ [1..4] $ \n -> do
        addLand (numbered n "Boros Guildgate")
        addLand (numbered n "Gateway Plaza")

  step "Cast Hunted Witness and sac it" $ do
    tapForMana "W" "Boros Guildgate 1"
    cast "W" "Hunted Witness"
    resolve "Hunted Witness"

    tapForMana "W" "Boros Guildgate 2"
    sacrificeToNecrolisk "Hunted Witness"
    withLocation (Active, Play)
      $ withAttribute lifelink
      $ addCreature (1, 1) "Soldier"

  step "Sac Oathsworn Vampire" $ do
    tapForMana "1" "Treasure"
    sacrifice "Treasure"
    sacrificeToNecrolisk "Oathsworn Vampire"

  step "Justice Strike soldier to trigger life gain" $ do
    tapForMana "W" "Treasure"
    sacrifice "Treasure"
    tapForMana "R" "Boros Guildgate 3"

    cast "RW" "Justice Strike"
    resolve "Justice Strike"
    target "Soldier"
    fight "Soldier" "Soldier"

  step "Cast Oathsworn Vampire from Graveyard, triggering Desecrated Tomb" $ do
    tapForMana "R" "Boros Guildgate 4"
    tapForMana "B" "Gateway Plaza 1"
    castFromLocation (Active, Graveyard) "1B" "Oathsworn Vampire"
    resolve "Oathsworn Vampire"

    trigger "Desecrated Tomb"
    withLocation (Active, Play)
      $ withAttribute flying
      $ addCreature (1, 1) "Bat"

  step "Sac vampire, bat, and plunderer" $ do
    tapForMana "B" "Gateway Plaza 2"
    sacrificeToNecrolisk "Oathsworn Vampire"

    tapForMana "1" "Treasure"
    sacrifice "Treasure"
    sacrificeToNecrolisk "Bat"

  step "Repeat vampire/bat cycle" $ do
    tapForMana "B" "Gateway Plaza 3"
    tapForMana "1" "Treasure"
    sacrifice "Treasure"
    castFromLocation (Active, Graveyard) "1B" "Oathsworn Vampire"
    resolve "Oathsworn Vampire"

    trigger "Desecrated Tomb"
    withLocation (Active, Play)
      $ withAttribute flying
      $ addCreature (1, 1) "Bat"

    tapForMana "B" "Gateway Plaza 4"
    sacrificeToNecrolisk "Oathsworn Vampire"

    tapForMana "1" "Treasure"
    sacrifice "Treasure"
    sacrificeToNecrolisk "Bat"

  step "Attack with Roc and Necrolisk" $ do
    validate "Roc Charger" $ matchAttribute "menace"
    validate "Undercity Necrolisk" $ matchAttribute "menace"

    attackWith ["Roc Charger", "Undercity Necrolisk"]

  fork $
    [ step "Roc is blocked" $ do
        gainAttribute "blocked" "Roc Charger"
        damagePlayer "Undercity Necrolisk"
        validateLife Opponent 0
    , step "Undercity Necrolisk is blocked" $ do
        gainAttribute "blocked" "Undercity Necrolisk"
        damagePlayer "Roc Charger"
        validateLife Opponent 0
    ]
