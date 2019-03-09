module Solutions.GuildsOfRavnica3 where

import Control.Monad
import Control.Monad.Except (catchError)

import Dovin.V1

-- This solution re-uses the "Treasure" card to avoid having to track a counter
-- for each new one created. This has the downside of requiring explicit
-- state-based action calls - a new one can't be created until the old one has
-- been removed. In hindsight, probably would be easier to follow by keeping a
-- running counter of treasures and refering to them separately.
solution :: GameMonad ()
solution = do
  let menace = "menace"
  let sacrificeToNecrolisk =
        \name -> do
          validateCanCastSorcery
          activate "1" "Undercity Necrolisk"
          validate name $
               matchInPlay
            <> matchController Active
            <> matchAttribute "creature"
          sacrifice name
          gainAttribute menace "Undercity Necrolisk"
          modifyStrength (1, 1) "Undercity Necrolisk"
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

    withLocation (Opponent, Play) $ do
      addCreature (4, 3) "Rekindling Phoenix 1"
      addCreature (4, 3) "Rekindling Phoenix 2"
      addCreature (4, 3) "Aurelia, Exemplar of Justice"

  step "Cast Hunted Witness and sac it" $ do
    tapForMana "W" "Boros Guildgate 1"
    cast "W" "Hunted Witness"
    resolve "Hunted Witness"

    tapForMana "W" "Boros Guildgate 2"
    sacrificeToNecrolisk "Hunted Witness"
    withLocation (Active, Play)
      $ withAttributes [lifelink, token]
      $ addCreature (1, 1) "Soldier"

  step "Sac Oathsworn Vampire" $ do
    withStateBasedActions $ do
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
      $ withAttributes [flying, token]
      $ addCreature (1, 1) "Bat"

  step "Sac vampire, bat, and plunderer" $ do
    tapForMana "B" "Gateway Plaza 2"
    sacrificeToNecrolisk "Oathsworn Vampire"

    withStateBasedActions $ do
      tapForMana "1" "Treasure"
      sacrifice "Treasure"
    sacrificeToNecrolisk "Bat"

  step "Repeat vampire/bat cycle" $ do
    withStateBasedActions $ do
      tapForMana "B" "Gateway Plaza 3"
      tapForMana "1" "Treasure"
      sacrifice "Treasure"

    castFromLocation (Active, Graveyard) "1B" "Oathsworn Vampire"
    resolve "Oathsworn Vampire"

    trigger "Desecrated Tomb"
    withLocation (Active, Play)
      $ withAttributes [flying, token]
      $ addCreature (1, 1) "Bat"

    tapForMana "B" "Gateway Plaza 4"
    sacrificeToNecrolisk "Oathsworn Vampire"

    withStateBasedActions $ do
      tapForMana "1" "Treasure"
      sacrifice "Treasure"
    sacrificeToNecrolisk "Bat"

  step "Attack with Roc and Necrolisk" $ do
    validate "Roc Charger" $ matchAttribute "menace"
    validate "Undercity Necrolisk" $ matchAttribute "menace"

    attackWith ["Roc Charger", "Undercity Necrolisk"]

  let blockers = ["Rekindling Phoenix 1", "Rekindling Phoenix 2"]

  fork $
    [ step "Roc is blocked" $ do
        combatDamage blockers "Roc Charger"
        combatDamage [] "Undercity Necrolisk"
        validateLife Opponent 0
    , step "Undercity Necrolisk is blocked" $ do
        combatDamage [] "Roc Charger"
        combatDamage blockers "Undercity Necrolisk"
        validateLife Opponent 0
    ]

whenMatch :: CardName -> CardMatcher -> GameMonad () -> GameMonad ()
whenMatch name f action = do
  match <- requireCard name f >> pure True `catchError` const (pure False)

  when match action

formatter _ = boardFormatter
