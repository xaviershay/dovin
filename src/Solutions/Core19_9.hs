module Solutions.Core19_9 where

import Dovin.V1
import Dovin.Prelude

spellCount = "spell-count"

-- Keeps track of number of spells cast, and if Aetherflux Reservoir is in play
-- triggers it.
castWithTriggers cost name = do
  modifying
    (counters . at spellCount . non 0)
    (+ 1)

  forCards
    (matchInPlay <> matchName "Aetherflux Reservoir")
    $ const $ do
      trigger "Aetherflux Reservoir"
      x <- use $ counters . at spellCount . non 0
      gainLife Active x

  cast cost name

solution :: GameMonad ()
solution = do
  step "Initial state" $ do
    setLife Opponent 50
    setLife Active 1

    withLocation (Active, Play) $ do
      addLands 3 "Spirebluff Canal"
      addArtifact "Cultivator's Caravan"
      addArtifact "Powerstone Shard 1"
      addArtifact "Powerstone Shard 2"
      addArtifact "Mox Amber 1"
      addCreature (2, 2) "Captain Lannery Storm"

    withLocation (Active, Hand) $ do
      addArtifact "Aetherflux Reservoir"
      addArtifact "Mox Amber 2"
      addInstant "Paradoxical Outcome"
      addArtifact "Foundry Inspector"
      addInstant "Abrade"

  step "Attack with Captain to get a Treasure, assume he's blocked and destroyed" $ do
    attackWith ["Captain Lannery Storm"]
    withLocation (Active, Play) $ withAttribute token $ addArtifact "Treasure"

    destroy "Captain Lannery Storm"
    transitionTo SecondMain

  step "Cast Foundry Inspector, reducing cost of future artifacts" $ do
    tapForMana "1" "Treasure"
    sacrifice "Treasure"
    tapForMana "U" "Spirebluff Canal 1"
    tapForMana "U" "Spirebluff Canal 2"
    castWithTriggers "3" "Foundry Inspector" >> resolveTop

  step "Cast Aetherflux Reservoir" $ do
    tapForMana "2" "Powerstone Shard 1"
    tapForMana "U" "Spirebluff Canal 3"
    castWithTriggers "3" "Aetherflux Reservoir" >> resolveTop

  step "Cast Mox Amber" $ do
    castWithTriggers "" "Mox Amber 2" >> resolveTop

  step "Tap Cultivator's Caravan, leave mana floating" $ do
    tapForMana "U" "Cultivator's Caravan"

  step "Cast Paradoxical Outcome for all artifacts and a tapped land" $ do
    tapForMana "2" "Powerstone Shard 2"
    tapForMana "U" "Mox Amber 1"
    tapForMana "U" "Mox Amber 2"
    castWithTriggers "3U" "Paradoxical Outcome" >> resolveTop

    moveTo Hand "Spirebluff Canal 1"
    moveTo Hand "Powerstone Shard 1"
    moveTo Hand "Powerstone Shard 2"
    moveTo Hand "Mox Amber 1"
    moveTo Hand "Mox Amber 2"
    moveTo Hand "Cultivator's Caravan"

  step "Replay land and moxes" $ do
    moveTo Play "Spirebluff Canal 1"
    forM_ [1..2] $ \n -> do
      castWithTriggers "" (numbered n "Mox Amber") >> resolveTop

  step "Replay Powerstone Shard 1 with moxes" $ do
    tapForMana "1" "Mox Amber 1"
    tapForMana "1" "Mox Amber 2"
    castWithTriggers "2" "Powerstone Shard 1" >> resolveTop

  step "Replay Powerstone Shard 2 with floating mana and land" $ do
    tapForMana "U" "Spirebluff Canal 1"
    castWithTriggers "2" "Powerstone Shard 2" >> resolveTop

  step "Replay Caravan" $ do
    tapForMana "2" "Powerstone Shard 1"
    castWithTriggers "2" "Cultivator's Caravan" >> resolveTop

  step "Cast Abrade, target doesn't matter" $ do
    tapForMana "R" "Cultivator's Caravan"
    tapForMana "2" "Powerstone Shard 2"
    castWithTriggers "1R" "Abrade" >> resolveTop

  step "Activate Aetherflux Reservoir, targeting opponent" $ do
    activate "" "Aetherflux Reservoir"
    loseLife Active 50
    damage (const 50) (targetPlayer Opponent) "Aetherflux Reservoir"
    validateLife Opponent 0

matchPowerstones = matchName "Powerstone Shard 1"
                    `matchOr` matchName "Powerstone Shard 2"
                   <> matchInPlay
                   <> matchController Active

matchArtifactMana = matchName "Mox Amber 1"
                    `matchOr` matchName "Mox Amber 2"
                    `matchOr` matchName "Cultivator's Caravan"
                    `matchOr` matchName "Treasure"

attributes = attributeFormatter $ do
  attribute "our life" $ countLife Active
  attribute "op. life" $ countLife Opponent
  attribute "spells" $ countValue spellCount
  attribute "mana" $ do
    normal <- (+)
                <$> countCards
                  ( matchAttribute "land"
                    `matchOr` matchArtifactMana
                  <> missingAttribute tapped
                  <> matchInPlay
                  <> matchController Active
                  )
                <*> countManaPool Active
    powerstones <- countCards matchPowerstones
    untapped    <- countCards $ matchPowerstones <> missingAttribute tapped
    return $ normal + (powerstones * untapped)

manaSources = cardFormatter
  "open mana sources"
  ( matchAttribute "land"
    `matchOr` matchArtifactMana
    `matchOr` matchPowerstones
  <> missingAttribute tapped
  <> matchInPlay
  <> matchController Active
  )
formatter 1 = attributes <> boardFormatter
formatter _ = attributes <> manaSources
