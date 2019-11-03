module Solutions.WarOfTheSpark2 where

import Dovin.V2
import Dovin.Prelude

dinosaur = "dinosaur"
tricked = "tricked"

solution :: GameMonad ()
solution = do
  step "Initial state" $ do
    as Opponent $ setLife 4

    withLocation Hand $ do
      addAura "Kaya's Ghostform"
      withAttribute haste $ addCreature (1, 1) "Dreadhorde Butcher"
      addCreature (2, 2) "Merfolk Trickster"
      addCreature (4, 4) "Massacre Girl"

    withLocation Play $ do
      addLands 4 "Steam Vents"
      addLands 4 "Blood Crypt"

      withPlusOneCounters 2 $ addCreature (2, 2) "Flux Channeler"
      withPlusOneCounters 3 $ withAttribute firststrike $ addCreature (1, 1) "Rigging Runner"
      withPlusOneCounters 1 $ addCreature (2, 2) "Spellgorger Weird"

    as Opponent $ do
      withLocation Play $ do
        addCreature (3, 2) "Midnight Reaper"
        addCreature (1, 1) "Drover of the Mighty"

        -- HACK: Model Drover's passive ability as an ability of Zetalpa. Don't
        -- currently have a way of checking _other_ cards to figure out if
        -- effect applies.
        withEffect
          matchInPlay
          (pure $ matchName "Drover of the Mighty" <> missingAttribute tricked)
          (pure . over cardStrengthModifier (mkStrength (2, 2) <>)) $
            withAttributes [indestructible] $
              addCreature (4, 8) "Zetalpa, Primal Dawn"
        addCreature (5, 5) "God-Eternal Rhonas"

  step "Cast Merfolk Trickster, targeting Drover" $ do
    tapForMana "U" "Steam Vents 1"
    tapForMana "U" "Steam Vents 2"

    cast "UU" "Merfolk Trickster" >> resolveTop

    target "Drover of the Mighty"
    gainAttribute tricked "Drover of the Mighty"
    tap "Drover of the Mighty"

  step "Cast Kaya's Ghostform on Merfolk Trickster" $ do
    tapForMana "B" "Blood Crypt 1"

    cast "B" "Kaya's Ghostform"
    target "Merfolk Trickster"

    trigger "Proliferate" "Flux Channeler"
    trigger "+1/+1" "Spellgorger Weird"

    resolve "+1/+1"
    modifyCard cardPlusOneCounters (+ 1) "Spellgorger Weird"

    resolve "Proliferate"
    modifyCard cardPlusOneCounters (+ 1) "Spellgorger Weird"
    modifyCard cardPlusOneCounters (+ 1) "Rigging Runner"
    modifyCard cardPlusOneCounters (+ 1) "Flux Channeler"

    resolve "Kaya's Ghostform"

  step "Cast Massacre Girl, killing Drover" $ do
    tapForMana "B" "Blood Crypt 2"
    tapForMana "B" "Blood Crypt 3"
    tapForMana "B" "Blood Crypt 4"
    tapForMana "U" "Steam Vents 3"
    tapForMana "U" "Steam Vents 4"

    cast "3BB" "Massacre Girl" >> resolveTop

    withStateBasedActions $ do
      forCards (matchInPlay <> matchAttribute creature <> invert (matchName "Massacre Girl")) $ \cn -> do
        modifyCard cardStrengthModifier (mkStrength (-1, -1) <>) cn

    validate (matchLocation (Opponent, Graveyard)) "Drover of the Mighty"

    trigger "-1/-1" "Massacre Girl"
    as Opponent $ do
      trigger "Self-damage" "Midnight Reaper"

  step "Resolve midnight reaper damage" $ do
    resolve "Self-damage"

    as Opponent $ loseLife 1

  step "Resolve -1/-1, killing Midnight Reaper and Trickster" $ do
    resolve "-1/-1"

    withStateBasedActions $ do
      forCards (matchInPlay <> matchAttribute creature <> invert (matchName "Massacre Girl")) $ \cn -> do
        modifyCard cardStrengthModifier (mkStrength (-1, -1) <>) cn

    -- Active player triggers stack first
    validate (matchLocation (Active, Graveyard)) "Merfolk Trickster"
    validate (matchLocation (Opponent, Graveyard)) "Midnight Reaper"
    moveTo Graveyard "Kaya's Ghostform"

    trigger "Return Trickster" "Kaya's Ghostform"
    trigger "-1/-1 Trickster" "Massacre Girl"
    trigger "-1/-1 Reaper" "Massacre Girl"
    as Opponent $ do
      trigger "Self-damage" "Midnight Reaper"

  step "Resolve midnight reaper damage" $ do
    resolve "Self-damage"

    as Opponent $ loseLife 1

  step "Resolve -1/-1 for Reaper" $ do
    resolve "-1/-1 Reaper"

    withStateBasedActions $ do
      forCards (matchInPlay <> matchAttribute creature <> invert (matchName "Massacre Girl")) $ \cn -> do
        modifyCard cardStrengthModifier (mkStrength (-1, -1) <>) cn

  step "Resolve -1/-1 for Trickster" $ do
    resolve "-1/-1 Trickster"

    withStateBasedActions $ do
      forCards (matchInPlay <> matchAttribute creature <> invert (matchName "Massacre Girl")) $ \cn -> do
        modifyCard cardStrengthModifier (mkStrength (-1, -1) <>) cn

  step "Return Trickster to play from Ghostform, targeting God-Eternal" $ do
    moveTo Play "Merfolk Trickster"

    target "God-Eternal Rhonas"
    tap "God-Eternal Rhonas"
    gainAttribute tricked "God-Eternal Rhonas"

  step "Attack with all original creatures, opponent can only block 1 and can't kill it" $ do
    attackWith ["Flux Channeler", "Rigging Runner", "Spellgorger Weird"]

    -- First strike damage
    combatDamage ["Zetalpa, Primal Dawn"] "Rigging Runner"

    -- Regular damage
    combatDamage [] "Flux Channeler"
    combatDamage [] "Spellgorger Weird"

    validateLife 0 Opponent

attributes = attributeFormatter $ do
  attribute "life" $ countLife Opponent
formatter _ = attributes <> boardFormatter
