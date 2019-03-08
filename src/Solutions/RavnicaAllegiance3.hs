module Solutions.RavnicaAllegiance3 where

import Dovin.V2
import Dovin.Prelude

solution :: GameMonad ()
solution = do
  step "Initial Setup" $ do
    setLife Opponent 15

    withLocation Play $ do
      addLands 2 "Godless Shrine"
      addLands 2 "Stomping Ground"
      addCreature (1, 1) "Elenda, the Dusk Rose"
      withAttribute legendary
        $ withEffect
           matchInPlay
           (matchOtherCreatures <> const (matchAttribute token))
           (pure . setAttribute vigilance . setAttribute lifelink)
        $ addCreature (2, 4) "Teysa Karlov"
      addCreature (2, 2) "Pitiless Pontiff"
      withAttribute defender $ addCreature (2, 3) "Novice Knight"
      withAttribute legendary
        $ withEffect
            matchInPlay
            matchOtherCreatures
            (pure . over cardStrength (mkStrength (1, 0) <>))
        $ addCreature (2, 2) "Judith, the Scourge Diva"

    withLocation Hand $ do
      withAttribute haste $ addCreature (3, 4) "Grand Warlord Radha"
      addInstant "Status // Statue"

    as Opponent $ do
      withLocation Play $ do
        -- Note: opponent has hexproof too! Not modeled here.
        withAttributes [flying]
          $ withEffect
              matchInPlay
              matchOtherCreatures
              (pure . setAttribute hexproof)
          $ addCreature (3, 4) "Shalai, Voice of Plenty"

        addCreature (2, 5) "Humongulus"
        addCreature (6, 6) "Rakdos, the Showstopper"

  step "Play Radha" $ do
    tapForMana "R" "Stomping Ground 1"
    tapForMana "G" "Stomping Ground 2"
    tapForMana "W" "Godless Shrine 1"
    tapForMana "W" "Godless Shrine 2"

    cast "2RW" "Grand Warlord Radha" >> resolveTop
  
  step "Attack with all except knight, generating mana" $ do
    attackWith
      [ "Judith, the Scourge Diva"
      , "Pitiless Pontiff"
      , "Teysa Karlov"
      , "Elenda, the Dusk Rose"
      , "Grand Warlord Radha"
      ]

    trigger "Generate Mana" "Grand Warlord Radha" >> resolveTop
    forCards
      (matchAttribute attacking)
      (const $ addMana "G")

  step "Status Judith to give her +1/+1 and deathtouch" $ do
    cast "G" "Status // Statue" >> resolveTop
    with "Judith, the Scourge Diva" $ \cn -> do
      gainAttribute deathtouch cn
      modifyCardDeprecated cn cardPlusOneCounters (+ 1)

  step "Sacrifice Knight to make Pontiff indestructible, ping Shalai and Rakdos with Judith" $ do
    activate "Indestructible" "1" "Pitiless Pontiff"
    sacrifice "Novice Knight"

    trigger "+1 1" "Elenda, the Dusk Rose"
    trigger "+1 2" "Elenda, the Dusk Rose"
    trigger "Judith for Elenda 1" "Judith, the Scourge Diva"
    trigger "Judith for Elenda 2" "Judith, the Scourge Diva"

    -- Need to put Shalai in the bin to remove hexproof from Rakdos
    withStateBasedActions $ do
      resolveTop
      damage (const 1) (targetCard "Shalai, Voice of Plenty") "Judith, the Scourge Diva"

    resolveTop
    damage (const 1) (targetCard "Rakdos, the Showstopper") "Judith, the Scourge Diva"

    resolveTop
    modifyCardDeprecated "Elenda, the Dusk Rose" cardPlusOneCounters (+ 1)
    resolveTop
    modifyCardDeprecated "Elenda, the Dusk Rose" cardPlusOneCounters (+ 1)

    resolve "Indestructible"
    gainAttribute indestructible "Pitiless Pontiff"
    gainAttribute deathtouch "Pitiless Pontiff"
    
  step "Apply combat damage, doesn't matter what Humongulus blocks, can't kill them" $ do
    combatDamage [] "Grand Warlord Radha"
    combatDamage [] "Judith, the Scourge Diva"
    combatDamage [] "Pitiless Pontiff"
    combatDamage [] "Teysa Karlov"
    combatDamage ["Humongulus"] "Elenda, the Dusk Rose"

  step "Sacrifice Radha, ping opponent (ignore Elenda triggers, irrelevant)" $ do
    activate "Indestructible" "1" "Pitiless Pontiff"
    sacrifice "Grand Warlord Radha"

    trigger "Judith for Elenda 1" "Judith, the Scourge Diva"
    trigger "Judith for Elenda 2" "Judith, the Scourge Diva"

    resolveTop
    damage (const 1) (targetPlayer Opponent) "Judith, the Scourge Diva"
    resolveTop
    damage (const 1) (targetPlayer Opponent) "Judith, the Scourge Diva"

    resolve "Indestructible"
    gainAttribute indestructible "Pitiless Pontiff"
    gainAttribute deathtouch "Pitiless Pontiff"

attributes = attributeFormatter $ do
  attribute "life" $ countLife Opponent
  attribute "pool" $ countManaPool Active
    
formatter _ = attributes <> boardFormatter
