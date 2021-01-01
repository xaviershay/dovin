module Solutions.RavnicaAllegiance4 where

import Dovin.V2

solution :: GameMonad ()
solution = do
  step "Initial state" $ do
    as Opponent $ setLife 13

    withLocation Play $ do
      addEnchantment "Rhythm of the Wild"
      addCreature (2, 2) "Combine Guildmage"
      addCreature (2, 3) "Herald of Secret Streams"
      addLands 4 "Breeding Pool"
      addLands 4 "Stomping Ground"

    withLocation Hand $ do
      addCreature (2, 2) "Zhur-Taa Goblin"
      withAttribute trample $ addCreature (3, 3) "Galloping Lizrog"
      addCreature (0, 2) "Incubation Druid"

  step "Activate Guildmage for extra +1/+1 counters" $ do
    tapForMana "G" "Stomping Ground 1"
    tapForMana "G" "Stomping Ground 2"
    tap "Combine Guildmage"
    activate "Extra +1/+1" "1G" "Combine Guildmage"
    resolveTop

  step "Cast Incubation Druid with haste from riot, +1/+1 from guildmage" $ do
    tapForMana "G" "Breeding Pool 1"
    tapForMana "G" "Breeding Pool 2"
    cast "1G" "Incubation Druid" >> resolveTop

    trigger "+1/+1" "Combine Guildmage"
    trigger "Riot" "Rhythm of the Wild"

    resolve "Riot"
    gainAttribute haste "Incubation Druid"

    resolve "+1/+1"
    modifyCardDeprecated "Incubation Druid" cardPlusOneCounters (+ 1)

  step "Cast Zhur-Taa, double Riot and bonus +1/+1 for 3 total" $ do
    tapForMana "G" "Stomping Ground 3"
    tapForMana "R" "Stomping Ground 4"

    cast "RG" "Zhur-Taa Goblin" >> resolveTop

    trigger "+1/+1" "Combine Guildmage"
    trigger "Rhythm Riot" "Rhythm of the Wild"
    trigger "Zhur-Taa Riot" "Zhur-Taa Goblin"

    resolve "Zhur-Taa Riot"
    modifyCardDeprecated "Zhur-Taa Goblin" cardPlusOneCounters (+ 1)

    resolve "Rhythm Riot"
    modifyCardDeprecated "Zhur-Taa Goblin" cardPlusOneCounters (+ 1)

    resolve "+1/+1"
    modifyCardDeprecated "Zhur-Taa Goblin" cardPlusOneCounters (+ 1)

  step "Tap Druid for 3 mana, add to lands to cast Lizrog" $ do
    tap "Incubation Druid" -- TODO: Check if this checks for haste?
    activate "Mana" "" "Incubation Druid" >> resolveTop

    addMana "GGG"
    tapForMana "U" "Breeding Pool 3"
    tapForMana "U" "Breeding Pool 4"

    cast "3UG" "Galloping Lizrog" >> resolveTop
    trigger "+1/+1 swap" "Galloping Lizrog"
    trigger "+1/+1" "Combine Guildmage"
    trigger "Rhythm Riot" "Rhythm of the Wild"

    resolve "Rhythm Riot"
    gainAttribute haste "Galloping Lizrog"

    resolve "+1/+1"
    modifyCardDeprecated "Galloping Lizrog" cardPlusOneCounters (+ 1)

    resolve "+1/+1 swap"
    modifyCardDeprecated "Galloping Lizrog" cardPlusOneCounters (\x -> x - 1)
    modifyCardDeprecated "Zhur-Taa Goblin" cardPlusOneCounters (\x -> x - 3)
    modifyCardDeprecated "Incubation Druid" cardPlusOneCounters (\x -> x - 1)

    modifyCardDeprecated "Galloping Lizrog" cardPlusOneCounters (+ (5 * 2))

  step "Attacking with Lizrog, unblockable from Herald" $ do
    validate matchInPlay "Herald of Secret Streams"

    attackWith ["Galloping Lizrog"]
    combatDamage [] "Galloping Lizrog"

    validateLife 0 Opponent

attributes = attributeFormatter $ do
  attribute "life" $ countLife Opponent

formatter _ = attributes <> boardFormatter
