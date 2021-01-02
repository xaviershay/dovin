module Solutions.CommanderLegends where

import Dovin.V3

import Data.Maybe (mapMaybe)

import Debug.Trace

import Control.Monad (forM_)
import Control.Monad.Except (catchError)

import qualified Data.Set as S

commander = "commander"

solution :: GameMonad ()
solution = do
  step "Initial state" $ do
    as (OpponentN 1) $ setLife 29
    as (OpponentN 2) $ setLife 23
    as (OpponentN 3) $ setLife 46

    as (OpponentN 2) $ do
      withLocation Play $
        addCreature (5, 4) "Frenzied Saddlebrute"

    as (OpponentN 3) $ do
      withLocation Play $ do
        addCreature (4, 4) "Sengir, the Dark Baron"
        withColors [Black, Green, Blue]
          $ withAttribute commander
          $ addCreature (2, 4) "Archelos, Lagoon Mystic"

    as Active $ do
      withLocation Hand $ do
        withAttribute flying $ addCreature (2, 2) "Malcolm, Keen-Eyed Navigator"
        addInstant "Soul's Fire"

      withLocation Command $ do
        withAttribute commander
          $ withColors [Red, Blue]
          $ addCreature (4, 4) "Kraum, Ludevic's Opus"

      withLocation Play $ do
        addLands 2 "Island"
        addLands 2 "Mountain"
        addLands 2 "Plains"

        addCreature (4, 4) "Port Razer"
        withAttribute commander
          $ withColors [White]
          $ addCreature (2, 2) "Ardenn, Intrepid Archaeologist"

        withOwner (OpponentN 3)
          $ withEffect
              -- TODO: Tidy all this up
              ((foldr (\cn m -> matchName cn `matchOr` m) matchNone
                . mapMaybe extractCardTarget) <$> viewSelf cardTargets)
              [ effectPTAdjust (3, 3)
              , effectProtectionF (const $ do
                  controller <- viewSelf cardOwner
                  commanders <- askCards
                    (matchController controller <> matchAttribute commander)

                  let colors =
                        foldl S.union mempty (map (view cardColors) commanders)
  
                  return (S.difference allColors colors)
              )
              ]
              "Equipped gets +3/+3 and pro colors NOT in commander colors"
              $ addArtifact "Commander's Plate"
        withCardTarget "Commander's Plate"
          $ withEffect
              ((foldr (\cn m -> matchName cn `matchOr` m) matchNone
                . mapMaybe extractCardTarget) <$> viewSelf cardTargets)
              [ effectSetControllerF (const $ viewSelf cardOwner)
              ]
              "Gain control of target"
          $ addAura "Confiscate"
        withCardTarget "Ardenn, Intrepid Archaeologist"
          $ withEffect
              -- TODO: Tidy all this up
              ((foldr
                (\cn m -> matchName cn `matchOr` m)
                (matchName "")
                . mapMaybe extractCardTarget) <$> viewSelf cardTargets)
              [ effectPTAdjust (2, 2)
              ]
              "Equipped gets +2/+2"
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

  step "Attack with Port Razer & Malcolm" $ do
    attackPlayerWith (OpponentN 3) 
      [ "Port Razer", "Malcolm, Keen-Eyed Navigator" ]

    trigger "Attacking angel" "Seraphic Greatsword" >> resolveTop
    -- TODO: validate player being attacked has most life

    withLocation Play
      $ withAttributes [token, tapped, attacking, flying]
      $ addCreature (4, 4) "Angel 1"

    forCards (matchController (OpponentN 3) <> matchCanBlock) $ \blocker -> do
      blockerColors <- view cardColors <$> requireCard blocker mempty

      forCards (matchAttribute attacking) $
        validate
          (matchAttribute flying `matchOr` matchProtectionAny blockerColors)
    forCards (matchAttribute attacking) $
      combatDamageTo (TargetPlayer $ OpponentN 3) []

    validateLife 31 (OpponentN 3)

    trigger "Another combat phase" "Port Razer" >> resolveTop

    transitionToForced BeginCombat

  step "Second combat, move Confiscate & Greatsword to Archon" $ do
    trigger "Ardenn Attach" "Ardenn, Intrepid Archaeologist" >> resolveTop

    attach "Confiscate" "Archon of Coronation"
    attach "Seraphic Greatsword" "Archon of Coronation"

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

matchCanBlock =
  matchInPlay <> matchAttribute creature <> missingAttribute tapped

matchProtectionAny =
  foldr
    matchOr
    matchNone
  . map matchProtection
  . S.toList

-- copy of 'attackWith', but allow haste-y attacking if a different player
-- controls Frenzied Saddlebrute
attackPlayerWith :: Player -> [CardName] -> GameMonad ()
attackPlayerWith player cs = do
  transitionTo DeclareAttackers

  hasteMatcher <-
    (do
      validate (matchInPlay <> invert (matchController player)) "Frenzied Saddlebrute"
      return matchAll
    ) `catchError` (const . return $ matchAttribute haste)

  forM_ cs $ \cn -> do
    c <- requireCard cn
           (matchInPlay
             <> matchAttribute "creature"
             <> missingAttribute "defender"
             <> labelMatch "does not have summoning sickness" (
                  hasteMatcher
                  `matchOr`
                  missingAttribute summoned
                ))
    forCards
      (matchName cn <> missingAttribute vigilance)
      -- Can't use 'tap' here because it checks summoning sickness.
      (gainAttribute tapped)
    gainAttribute attacking cn
