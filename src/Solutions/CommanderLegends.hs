module Solutions.CommanderLegends where 
import Dovin.V3

import Data.Maybe (mapMaybe)

import Debug.Trace

import Control.Monad (forM_)
import Control.Monad.Except (catchError, throwError)

import qualified Data.Set as S

commander = "commander"

solution :: GameMonad ()
solution = do
  step "Initial state" $ do
    as (OpponentN 1) $ setLife 29
    as (OpponentN 2) $ setLife 23
    as (OpponentN 3) $ setLife 46

    as (OpponentN 1) $ do
      withZone Play $
        withAttribute flying
        $ addCreature (5, 5) "Archon of Coronation"

    as (OpponentN 2) $ do
      withZone Play $ do
        withColors [Red]
          $ addCreature (5, 4) "Frenzied Saddlebrute"
        withColors [Red, White]
          $ addCreature (7, 5) "Dargo, the Shipwrecker"

    as (OpponentN 3) $ do
      withZone Play $ do
        addCreature (4, 4) "Sengir, the Dark Baron"
        withColors [Black, Green, Blue]
          $ withAttribute commander
          $ addCreature (2, 4) "Archelos, Lagoon Mystic"

    as Active $ do
      withZone Hand $ do
        withAttribute flying $ addCreature (2, 2) "Malcolm, Keen-Eyed Navigator"
        addInstant "Soul's Fire"
        addInstant "Wrong Turn"

      withZone Command $ do
        withAttribute commander
          $ withColors [Red, Blue]
          $ addCreature (4, 4) "Kraum, Ludevic's Opus"

      withZone Play $ do
        addLands 2 "Island"
        addLands 2 "Mountain"
        addLands 2 "Plains"

        addCreature (4, 4) "Port Razer"
        withAttribute commander
          $ withColors [White]
          $ addCreature (2, 2) "Ardenn, Intrepid Archaeologist"

        as (OpponentN 3)
          $ withEffect
              -- TODO: Tidy all this up
              ((foldr (\cn m -> matchName cn `matchOr` m) matchNone
                . mapMaybe extractCardTarget) <$> viewSelf cardTargets)
              [ effectPTAdjust (3, 3)
              , effectProtectionF (const $ do
                  controller <- viewSelf cardController
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
               [ effectControlF (const $ viewSelf cardController)
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
    transitionTo DeclareAttackers

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

    trigger "Another combat phase" "Port Razer"
    trigger "Treasures" "Malcolm, Keen-Eyed Navigator"

    resolveTop
    withZone Play $ addArtifact "Treasure 1"

    resolveTop
    forCards (matchInPlay <> matchController Active) $ \cn -> do
      loseAttribute attacking cn
      loseAttribute tapped cn

    transitionToForced BeginCombat

  step "Second combat, move Confiscate & Greatsword to Archon" $ do
    trigger "Ardenn Attach" "Ardenn, Intrepid Archaeologist" >> resolveTop

    attach "Confiscate" "Archon of Coronation"
    gainAttribute summoned "Archon of Coronation"
    attach "Seraphic Greatsword" "Archon of Coronation"

  step "Attack P1 with Ardenn" $ do
    transitionTo DeclareAttackers

    -- Technically out of order, but simplies things a bit
    trigger "Attacking angel" "Seraphic Greatsword" >> resolveTop
    -- TODO: validate player being attacked has most life

    withLocation Play
      $ withAttributes [token, tapped, attacking, flying]
      $ addCreature (4, 4) "Angel 2"

    let attackingP1 = [ "Ardenn, Intrepid Archaeologist" ]
    let attackingP2 = [ "Port Razer" ]
    let attackingP3 =
          [ "Archon of Coronation"
          , "Angel 1"
          , "Angel 2"
          , "Malcolm, Keen-Eyed Navigator"
          ]

    attackPlayerWith (OpponentN 1) attackingP1
    attackPlayerWith (OpponentN 2) attackingP2
    attackPlayerWith (OpponentN 3) attackingP3

    forCards (matchController (OpponentN 1) <> matchCanBlock) $ \blocker -> do
      blockerColors <- view cardColors <$> requireCard blocker mempty

      forM_ attackingP1 $
        validate (matchAttribute flying `matchOr` matchProtectionAny blockerColors)

    forCards (matchController (OpponentN 2) <> matchCanBlock) $ \blocker -> do
      blockerColors <- view cardColors <$> requireCard blocker mempty

      forM_ attackingP2 $
        validate (matchAttribute flying `matchOr` matchProtectionAny blockerColors)

    forCards (matchController (OpponentN 3) <> matchCanBlock) $ \blocker -> do
      blockerColors <- view cardColors <$> requireCard blocker mempty

      forM_ attackingP3 $
        validate (matchAttribute flying `matchOr` matchProtectionAny blockerColors)

    forM_ attackingP1 $ combatDamageTo (TargetPlayer $ OpponentN 1) []
    forM_ attackingP2 $ combatDamageTo (TargetPlayer $ OpponentN 2) []
    forM_ attackingP3 $ combatDamageTo (TargetPlayer $ OpponentN 3) []

    trigger "Another combat phase" "Port Razer"
    trigger "Treasures" "Malcolm, Keen-Eyed Navigator"

    validateLife 27 (OpponentN 1)
    validateLife 16 (OpponentN 2)
    validateLife 14 (OpponentN 3)

  step "Resolve combat triggers" $ do
    -- TODO: Automatically figure out how many there should be, or validate
    resolve "Treasures"
    withZone Play $ addArtifact "Treasure 2"
    withZone Play $ addArtifact "Treasure 3"

    tapForMana "U" "Treasure 1"
    tapForMana "U" "Treasure 2"
    tapForMana "U" "Treasure 3"

    sacrifice "Treasure 1"
    sacrifice "Treasure 2"
    sacrifice "Treasure 3"

    cast "2U" "Wrong Turn" >> resolveTop

    addEffect (effectControl (OpponentN 1)) "Archon of Coronation"
    gainAttribute summoned "Archon of Coronation"

    resolve "Another combat phase"
    forCards (matchInPlay <> matchController Active) $ \cn -> do
      loseAttribute attacking cn
      loseAttribute tapped cn

    transitionToForced BeginCombat

  step "Third combat, move Confiscate & Greatsword to Saddlebrute" $ do
    trigger "Ardenn Attach" "Ardenn, Intrepid Archaeologist" >> resolveTop

    attach "Confiscate" "Frenzied Saddlebrute"
    gainAttribute summoned "Frenzied Saddlebrute"
    attach "Seraphic Greatsword" "Frenzied Saddlebrute"

  step "Third combat" $ do
    transitionTo DeclareAttackers

    -- Technically out of order, but simplies things a bit
    trigger "Attacking angel" "Seraphic Greatsword" >> resolveTop
    -- TODO: validate player being attacked has most life

    withLocation Play
      $ withAttributes [token, tapped, attacking, flying]
      $ addCreature (4, 4) "Angel 3"

    let attackingP1 =
          [ "Port Razer"
          , "Frenzied Saddlebrute"
          , "Ardenn, Intrepid Archaeologist"
          , "Angel 3"
          ]
    let attackingP2 = [ ]
    let attackingP3 =
          [ "Angel 1"
          , "Angel 2"
          , "Malcolm, Keen-Eyed Navigator"
          ]

    attackPlayerWith (OpponentN 1) attackingP1
    attackPlayerWith (OpponentN 2) attackingP2
    attackPlayerWith (OpponentN 3) attackingP3

    forCards (matchController (OpponentN 1) <> matchCanBlock) $ \blocker -> do
      blockerColors <- view cardColors <$> requireCard blocker mempty

      forM_ attackingP1 $
        validate (matchAttribute flying `matchOr` matchProtectionAny blockerColors)

    forCards (matchController (OpponentN 2) <> matchCanBlock) $ \blocker -> do
      blockerColors <- view cardColors <$> requireCard blocker mempty

      forM_ attackingP2 $
        validate (matchAttribute flying `matchOr` matchProtectionAny blockerColors)

    forCards (matchController (OpponentN 3) <> matchCanBlock) $ \blocker -> do
      blockerColors <- view cardColors <$> requireCard blocker mempty

      forM_ attackingP3 $
        validate (matchAttribute flying `matchOr` matchProtectionAny blockerColors)

    forM_ attackingP1 $ combatDamageTo (TargetPlayer $ OpponentN 1) []
    forM_ attackingP2 $ combatDamageTo (TargetPlayer $ OpponentN 2) []
    forM_ attackingP3 $ combatDamageTo (TargetPlayer $ OpponentN 3) []

    trigger "Another combat phase" "Port Razer"
    trigger "Treasures" "Malcolm, Keen-Eyed Navigator"

    validateLife 7 (OpponentN 1)
    validateLife 16 (OpponentN 2)
    validateLife 4 (OpponentN 3)

  step "Resolve combat triggers" $ do
    -- TODO: Automatically figure out how many there should be, or validate
    resolve "Treasures"
    withZone Play $ addArtifact "Treasure 4"
    withZone Play $ addArtifact "Treasure 5"

    resolve "Another combat phase"
    forCards (matchInPlay <> matchController Active) $ \cn -> do
      loseAttribute attacking cn
      loseAttribute tapped cn

    transitionToForced BeginCombat

  step "Fourth combat, move Greatsword to Angel" $ do
    trigger "Ardenn Attach" "Ardenn, Intrepid Archaeologist" >> resolveTop
    attach "Seraphic Greatsword" "Angel 1"

  step "Fourth combat" $ do
    transitionTo DeclareAttackers

    -- Technically out of order, but simplies things a bit
    trigger "Attacking angel" "Seraphic Greatsword" >> resolveTop
    -- TODO: validate player being attacked has most life

    withLocation Play
      $ withAttributes [token, tapped, attacking, flying]
      $ addCreature (4, 4) "Angel 4"

    let attackingP1 =
          [ "Frenzied Saddlebrute"
          , "Ardenn, Intrepid Archaeologist"
          ]
    let attackingP2 =
          [ "Angel 1"
          , "Angel 2"
          , "Angel 4"
          , "Malcolm, Keen-Eyed Navigator"
          ]
    let attackingP3 =
          [ "Angel 3"
          ]

    attackPlayerWith (OpponentN 1) attackingP1
    attackPlayerWith (OpponentN 2) attackingP2
    attackPlayerWith (OpponentN 3) attackingP3

    forCards (matchController (OpponentN 1) <> matchCanBlock) $ \blocker -> do
      blockerColors <- view cardColors <$> requireCard blocker mempty

      forM_ attackingP1 $
        validate (matchAttribute flying `matchOr` matchProtectionAny blockerColors)

    forCards (matchController (OpponentN 2) <> matchCanBlock) $ \blocker -> do
      blockerColors <- view cardColors <$> requireCard blocker mempty

      forM_ attackingP2 $
        validate (matchAttribute flying `matchOr` matchProtectionAny blockerColors)

    forCards (matchController (OpponentN 3) <> matchCanBlock) $ \blocker -> do
      blockerColors <- view cardColors <$> requireCard blocker mempty

      forM_ attackingP3 $
        validate (matchAttribute flying `matchOr` matchProtectionAny blockerColors)

    forM_ attackingP1 $ combatDamageTo (TargetPlayer $ OpponentN 1) []
    forM_ attackingP2 $ combatDamageTo (TargetPlayer $ OpponentN 2) []
    forM_ attackingP3 $ combatDamageTo (TargetPlayer $ OpponentN 3) []

    trigger "Another combat phase" "Port Razer"
    trigger "Treasures" "Malcolm, Keen-Eyed Navigator"

    validateLife 0 (OpponentN 1)
    validateLife 0 (OpponentN 2)
    validateLife 0 (OpponentN 3)

attach cn tn = do
  c <- requireCard cn $ matchInPlay
  
  validate matchInPlay tn

  modifyCard cardTargets (const [TargetCard tn]) cn

extractCardTarget (TargetCard cn) = Just cn
extractCardTarget _ = Nothing

attributes = attributeFormatter $ do
  attribute "life #1" $ countLife (OpponentN 1)
  attribute "life #2" $ countLife (OpponentN 2)
  attribute "life #3" $ countLife (OpponentN 3)

formatter _ = attributes <> boardFormatter2

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
