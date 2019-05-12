module Solutions.WarOfTheSpark2 where

import Dovin.V2
import Dovin.Prelude

dinosaur = "dinosaur"

solution :: GameMonad ()
solution = do
  step "Initial state" $ do
    setLife Opponent 4

    withLocation Hand $ do
      addAura "Kaya's Ghostform"
      withAttribute haste $ addCreature (1, 1) "Dreadhorde Butcher"
      addCreature (4, 4) "Massacre Girl"

    withLocation Play $ do
      addLands 4 "Steam Vents"
      addLands 4 "Blood Crypt"

      withPlusOneCounters 2 $ addCreature (2, 2) "Flux Channeler"
      withPlusOneCounters 3 $ addCreature (1, 1) "Rigging Runner"
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
          (pure $ matchName "Drover of the Mighty")
          (pure . over cardStrengthModifier (mkStrength (2, 2) <>)) $
            withAttributes [indestructible] $
              addCreature (4, 8) "Zetalpa, Primal Dawn"
        addCreature (5, 5) "God-Eternal Rhonas"

  step "Cast Dreadhorde Butcher" $ do
    tapForMana "R" "Steam Vents 1"
    tapForMana "B" "Blood Crypt 1"

    cast "RB" "Dreadhorde Butcher" >> resolveTop

  step "Cast Kaya's Ghostform on Dreadhorde Butcher" $ do
    tapForMana "B" "Blood Crypt 2"

    cast "B" "Kaya's Ghostform"

    trigger "Proliferate" "Flux Channeler"
    trigger "+1/+1" "Spellgorger Weird"

    resolve "+1/+1"
    modifyCard cardPlusOneCounters (+ 1) "Spellgorger Weird"

    resolve "Proliferate"
    modifyCard cardPlusOneCounters (+ 1) "Spellgorger Weird"
    modifyCard cardPlusOneCounters (+ 1) "Rigging Runner"
    modifyCard cardPlusOneCounters (+ 1) "Flux Channeler"

    resolve "Kaya's Ghostform"


attributes = attributeFormatter $ do
  attribute "life" $ countLife Opponent
formatter _ = attributes <> boardFormatter
