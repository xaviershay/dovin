module Solutions.RavnicaAllegiancePre2 where

import Dovin.V2
import Dovin.Prelude

import Data.List (delete)
import Control.Lens
import Debug.Trace

solution :: GameMonad ()
solution = do
  step "Initial Setup" $ do
    setLife Opponent 7

    withLocation Play $ do
      addLands 3 "Blood Crypt"
      addLands 3 "Overgrown Tomb"
      withAttribute flying $ addCreature (3, 3) "Mausoleum Harpy"
      withAttribute defender $ addCreature (4, 3) "Piston-Fist Cyclops"
      withAttribute defender $ addCreature (0, 2) "Dragon Egg"
      addEnchantment "Rhythm of the Wild 1"
      addEnchantment "Rhythm of the Wild 2"

    withLocation Hand $ do
      addInstant "Ancient Animus"
      addCreature (2, 2) "Growth-Chamber Guardian 1"

    withLocation Deck $ do
      addCreature (2, 2) "Growth-Chamber Guardian 2"

    as Opponent $ do
      withLocation Play $ do
        withAttribute doublestrike $ addCreature (3, 3) "Goring Ceratops"
        withAttribute firststrike $ addCreature (3, 3) "Goblin Chainwhirler 1"
        withAttribute firststrike $ addCreature (3, 3) "Goblin Chainwhirler 2"

  step "Cast Guardian, get haste/+1 from Rhythm triggers" $ do
    tapForMana "G" "Overgrown Tomb 1"
    tapForMana "R" "Blood Crypt 1"
    cast "1G" "Growth-Chamber Guardian 1" >> resolveTop

    trigger "Haste" "Rhythm of the Wild 1"
    trigger "+1/+1" "Rhythm of the Wild 2"
    resolveTop
    modifyCard "Growth-Chamber Guardian 1" cardPlusOneCounters (+ 1)
    trigger "Search" "Growth-Chamber Guardian 1"

    resolveTop
    move (Active, Deck) (Active, Hand) "Growth-Chamber Guardian 2"

    resolveTop
    gainAttribute haste "Growth-Chamber Guardian 1"

  step "Cast second Guardian, same haste/+1 combo (decline search)" $ do
    tapForMana "G" "Overgrown Tomb 2"
    tapForMana "R" "Blood Crypt 2"
    cast "1G" "Growth-Chamber Guardian 2" >> resolveTop

    trigger "Haste" "Rhythm of the Wild 1"
    trigger "+1/+1" "Rhythm of the Wild 2"
    resolveTop
    modifyCard "Growth-Chamber Guardian 2" cardPlusOneCounters (+ 1)
    trigger "Search" "Growth-Chamber Guardian 1"
    resolveTop

    resolveTop
    gainAttribute haste "Growth-Chamber Guardian 2"

  step "Cast Animus on Egg. Enables Piston-Fist, triggers Harpy" $ do
    tapForMana "G" "Overgrown Tomb 3"
    tapForMana "R" "Blood Crypt 3"
    cast "1G" "Ancient Animus" >> resolveTop
    target "Dragon Egg"

    loseAttribute defender "Piston-Fist Cyclops"

    fight "Dragon Egg" "Goring Ceratops"
    trigger "Make Dragon" "Dragon Egg"
    trigger "+1/+1" "Mausoleum Harpy"

    resolveHarpyTrigger

    resolveTop
    withLocation Play $ do
      withAttributes [token, flying, summoned] $ addCreature (2, 2) "Dragon"

  step "Attack with all except dragon" $ do
    attackWith
      [ "Mausoleum Harpy"
      , "Piston-Fist Cyclops"
      , "Growth-Chamber Guardian 1"
      , "Growth-Chamber Guardian 2"
      ]

  fork
    [ step "Opponent blocks all on ground, first-strike damage pumps harpy" $ do
        step "First-strike damage" $ do
          combatDamage ["Goring Ceratops"] "Piston-Fist Cyclops"
          combatDamage ["Goblin Chainwhirler 1"] "Growth-Chamber Guardian 1"
          combatDamage ["Goblin Chainwhirler 2"] "Growth-Chamber Guardian 2"

        step "Death triggers for harpy" $ do
          trigger "Cyclops" "Mausoleum Harpy"
          trigger "Guardian 1" "Mausoleum Harpy"
          trigger "Guardian 2" "Mausoleum Harpy"

          resolveHarpyTrigger
          resolveHarpyTrigger
          resolveHarpyTrigger

        step "Regular damage" $ do
          combatDamage [] "Mausoleum Harpy"
          validateLife 0 Opponent
    , step "Opponent doesn't block, damage is lethal" $ do
        step "First-strike damage" $ do
          combatDamage ["Goring Ceratops"] "Piston-Fist Cyclops"
          combatDamage ["Goblin Chainwhirler 1"] "Growth-Chamber Guardian 1"

        step "Death triggers for harpy" $ do
          trigger "Cyclops" "Mausoleum Harpy"
          trigger "Guardian 1" "Mausoleum Harpy"

          resolveHarpyTrigger
          resolveHarpyTrigger

        step "Regular damage" $ do
          combatDamage [] "Mausoleum Harpy"
          combatDamage [] "Growth-Chamber Guardian 2"
          validateLife (-2) Opponent
    ]

formatter 1 = attributes <> boardFormatter
formatter 5 = attributes <>
  cardFormatter
    "attacking creatures"
    (matchLocation (Active, Play)
    <> matchAttribute attacking
    )
formatter _ = attributes

resolveHarpyTrigger = do
  resolveTop
  modifyCard "Mausoleum Harpy" cardPlusOneCounters (+ 1)

attributes = attributeFormatter $ do
  attribute "life" $ countLife Active
  attribute "mana" $ countCards $ matchAttribute land <> missingAttribute tapped
  attribute "harpy" $
    view cardStrength <$> requireCard "Mausoleum Harpy" mempty

blocked = "blocked"
