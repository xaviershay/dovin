module Solutions.CommanderLegends where

import Dovin.V2
import Dovin.Prelude

import Data.Maybe (mapMaybe)

solution :: GameMonad ()
solution = do
  step "Initial state" $ do
    as (OpponentN 1) $ setLife 29
    as (OpponentN 2) $ setLife 23
    as (OpponentN 3) $ setLife 46

    as (OpponentN 3) $ do
      withLocation Play $
        addCreature (4, 4) "Sengir, the Dark Baron"

    as Active $ do
      withLocation Hand $ do
        addCreature (2, 2) "Malcolm, Keen-Eyed Navigator"
        addInstant "Soul's Fire"

      withLocation Play $ do
        addLands 2 "Island"
        addLands 2 "Mountain"
        addLands 2 "Plains"

        addCreature (4, 4) "Port Razer"
        addCreature (2, 2) "Ardenn, Intrepid Archaeologist"

        withOwner (OpponentN 3)
          -- $ withTarget (TargetCard "Test")
          $ withEffect
              matchInPlay
              -- TODO: Tidy all this up
              (foldr
                (\cn m -> matchName cn `matchOr` m)
                (matchName "")
                . mapMaybe extractCardTarget . view cardTargets)
              (pure . over cardStrengthModifier (mkStrength (3, 3) <>))
              $ addArtifact "Commander's Plate"
        withTarget (TargetCard "Commander's Plate") $ addAura "Confiscate"
        withTarget (TargetCard "Ardenn, Intrepid Archaeologist")
          $ withEffect
              matchInPlay
              -- TODO: Tidy all this up
              (foldr
                (\cn m -> matchName cn `matchOr` m)
                (matchName "")
                . mapMaybe extractCardTarget . view cardTargets)
              (pure . over cardStrengthModifier (mkStrength (2, 2) <>))
              $ addArtifact "Seraphic Greatsword"


  step "Cast Malcolm" $ do
    tapForMana "U" "Island 1"
    tapForMana "W" "Plains 1"
    tapForMana "W" "Plains 2"

    cast "2U" "Malcolm, Keen-Eyed Navigator" >> resolveTop

  step "Soul's Fire to kill Baron" $ do
    tapForMana "R" "Mountain 1"
    tapForMana "R" "Mountain 2"
    tapForMana "U" "Island 2"

    cast "2R" "Soul's Fire" >> resolveTop

    target "Port Razer"
    damage (view cardPower) (TargetCard "Sengir, the Dark Baron") "Port Razer"

  step "First Combat, move Plate & Greatsword to Port Razer" $ do
    transitionTo BeginCombat

    trigger "Ardenn Attach" "Ardenn, Intrepid Archaeologist" >> resolveTop

    attach "Commander's Plate" "Port Razer"
    attach "Seraphic Greatsword" "Port Razer"

attach cn tn = do
  c <- requireCard cn $ matchInPlay

  modifyCard cardTargets (const [TargetCard tn]) cn

extractCardTarget (TargetCard cn) = Just cn
extractCardTarget _ = Nothing

attributes = attributeFormatter $ do
  attribute "life #1" $ countLife (OpponentN 1)
  attribute "life #2" $ countLife (OpponentN 2)
  attribute "life #3" $ countLife (OpponentN 3)

formatter _ = attributes <> boardFormatter
