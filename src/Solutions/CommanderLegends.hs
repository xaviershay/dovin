module Solutions.CommanderLegends where
import Dovin.V4

import Data.Maybe (mapMaybe)

import Control.Monad (forM_, foldM, when)
import Control.Monad.Except (catchError)
import Control.Lens (modifying, at, non, use, assign, Lens')

import qualified Data.Set as S

solution :: GameMonad ()
solution = do
  step "Initial state" $ do
    as Active $ setLife 10
    as (OpponentN 1) $ setLife 29
    as (OpponentN 2) $ setLife 23
    as (OpponentN 3) $ setLife 46

    as (OpponentN 1) $
      withZone Play $
        withAttribute flying
        $ addCreature (5, 5) "Archon of Coronation"

    as (OpponentN 2) $
      withZone Play $ do
        withColors [Red]
          $ addCreature (5, 4) "Frenzied Saddlebrute"
        withColors [Red, White]
          $ addCreature (7, 5) "Dargo, the Shipwrecker"

    as (OpponentN 3) $
      withZone Play $ do
        addCreature (4, 4) "Sengir, the Dark Baron"
        withColors [Black, Green, Blue]
          $ withAttribute commander
          $ addCreature (2, 4) "Archelos, Lagoon Mystic"

        withEffect
          matchAttached
          [ effectPTAdjust (3, 3)
          , effectProtectionF (const $ do
              controller <- viewSelf cardController
              commanders <- askCards
                (matchController controller <> matchAttribute commander)

              let colors =
                    foldl S.union mempty (map (view cardColors) commanders)

              return $ S.difference allColors colors
          )
          ]
          "Attached gets +3/+3 and pro colors NOT in commander colors"
          $ addArtifact "Commander's Plate"

    as Active $ do
      withZone Hand $ do
        withAttributes [pirate, flying]
          $ addCreature (2, 2) "Malcolm, Keen-Eyed Navigator"
        addInstant "Soul's Fire"
        addInstant "Wrong Turn"

      withZone Command
        $ withAttribute commander
          $ withColors [Red, Blue]
          $ addCreature (4, 4) "Kraum, Ludevic's Opus"

      withZone Play $ do
        addLands 2 "Island"
        addLands 2 "Mountain"
        addLands 2 "Plains"

        withAttribute pirate $ addCreature (4, 4) "Port Razer"
        withAttribute commander
          $ withColors [White]
          $ addCreature (2, 2) "Ardenn, Intrepid Archaeologist"

        withCardTarget "Commander's Plate"
           $ withEffect
              matchAttached
               [ effectControlF (const $ viewSelf cardController)
               ]
               "Gain control of target"
          $ addAura "Confiscate"

        withCardTarget "Ardenn, Intrepid Archaeologist"
          $ withEffect
              matchAttached
              [ effectPTAdjust (2, 2)
              ]
              "Attached gets +2/+2"
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

  step "Attack P3 with Port Razer & Malcolm" $ do
    transitionTo DeclareAttackers

    attackPlayerTo (OpponentN 3) 31
      [ "Port Razer", "Malcolm, Keen-Eyed Navigator" ]

  step "Resolve combat triggers to untap attackers and creature treasure" $ do
    trigger "Another combat phase" "Port Razer"
    trigger "Treasures" "Malcolm, Keen-Eyed Navigator"

    resolveMalcolmTrigger

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

  step "Attack P1 with Ardenn, P2 with Razer, P3 with Arcon and Malcolm" $ do
    transitionTo DeclareAttackers

    attackPlayerTo (OpponentN 1) 27
      [ "Ardenn, Intrepid Archaeologist" ]
    attackPlayerTo (OpponentN 2) 16
      [ "Port Razer" ]
    attackPlayerTo (OpponentN 3) 14
      [ "Archon of Coronation"
      , "Angel 1"
      , "Malcolm, Keen-Eyed Navigator"
      ]

    trigger "Another combat phase" "Port Razer"
    trigger "Treasures" "Malcolm, Keen-Eyed Navigator"

  step "Create treasures from Malcomn's trigger" resolveMalcolmTrigger

  step "Cast Wrong Turn to give Archon back to P1" $ do
    tapForMana "U" "Treasure 1"
    tapForMana "U" "Treasure 2"
    tapForMana "U" "Treasure 3"

    sacrifice "Treasure 1"
    sacrifice "Treasure 2"
    sacrifice "Treasure 3"

    cast "2U" "Wrong Turn" >> resolveTop

    addEffect (effectControl (OpponentN 1)) "Archon of Coronation"
    gainAttribute summoned "Archon of Coronation"

  step "Untap and another combat phase from Port Razer's trigger"
    resolvePortRazerTrigger

  step "Third combat, move Confiscate & Greatsword to Saddlebrute" $ do
    trigger "Ardenn Attach" "Ardenn, Intrepid Archaeologist" >> resolveTop

    attach "Confiscate" "Frenzied Saddlebrute"
    gainAttribute summoned "Frenzied Saddlebrute"
    attach "Seraphic Greatsword" "Frenzied Saddlebrute"

  step "Attack P1 with Razer, Ardenn & Saddlebrute, P3 with Angels & Malcolm" $ do
    transitionTo DeclareAttackers

    attackPlayerTo (OpponentN 1) 7
      [ "Port Razer"
      , "Frenzied Saddlebrute"
      , "Ardenn, Intrepid Archaeologist"
      ]
    attackPlayerTo (OpponentN 3) 4
      [ "Angel 1"
      , "Angel 2"
      , "Malcolm, Keen-Eyed Navigator"
      ]

    trigger "Another combat phase" "Port Razer"
    trigger "Treasures" "Malcolm, Keen-Eyed Navigator"

  step "Create treasures from Malcomn's trigger (unused)" resolveMalcolmTrigger

  step "Untap and another combat phase from Port Razer's trigger"
    resolvePortRazerTrigger

  step "Fourth combat, move Greatsword to Angel" $ do
    trigger "Ardenn Attach" "Ardenn, Intrepid Archaeologist" >> resolveTop
    attach "Seraphic Greatsword" "Angel 1"

  step "Attack P1 with Ardenn & Saddlebrute, P3 with angel, P2 with rest" $ do
    transitionTo DeclareAttackers

    attackPlayerTo (OpponentN 1) 0
      [ "Frenzied Saddlebrute"
      , "Ardenn, Intrepid Archaeologist"
      ]
    attackPlayerTo (OpponentN 2) 0
      [ "Angel 1"
      , "Angel 2"
      , "Malcolm, Keen-Eyed Navigator"
      ]
    attackPlayerTo (OpponentN 3) 0
      [ "Angel 3"
      ]

resolvePortRazerTrigger = do
  resolve "Another combat phase"
  forCards (matchInPlay <> matchController Active) $ \cn -> do
    loseAttribute attacking cn
    loseAttribute tapped cn

  transitionToForced BeginCombat

resolveMalcolmTrigger = do
  resolve "Treasures"
  n <- use malcolmCounter

  forM_ (replicate n undefined) . const $ do
    modifying treasureCounter (+ 1)

    n <- use treasureCounter

    let name = "Treasure " <> show n

    withZone Play $ withAttribute token $ addArtifact name

  assign malcolmCounter 0

attach cn tn = do
  c <- requireCard cn matchInPlay

  validate matchInPlay tn

  modifyCard cardTargets (const [TargetCard tn]) cn

matchCanBlock =
  matchInPlay <> matchAttribute creature <> missingAttribute tapped

-- TODO: This probably belongs in core, let's find some other uses first
-- though.
matchAttached :: EffectMonad CardMatcher
matchAttached =
  matchAny matchName . mapMaybe extractCardTarget <$> viewSelf cardTargets

  where
    extractCardTarget (TargetCard cn) = Just cn
    extractCardTarget _ = Nothing

attackPlayerTo :: Player -> Int -> [CardName] -> GameMonad ()
attackPlayerTo opponent expectedLife attackers = do
  attackPlayerWith opponent attackers

  finalAttackers <- do
    opLife <- countLife opponent
    maxLife <- maximum <$> mapM countLife allPlayers

    let mostLife = opLife == maxLife

    swordAttacking <- check
                        (matchAny matchTarget (map TargetCard attackers))
                        "Seraphic Greatsword"

    if mostLife && swordAttacking then do
      trigger "Attacking angel" "Seraphic Greatsword" >> resolveTop

      modifying angelCounter (+ 1)

      n <- use angelCounter

      let name = "Angel " <> show n

      withZone Play
        $ withAttributes [token, tapped, attacking, flying]
        $ addCreature (4, 4) name

      return (name:attackers)
    else
      return attackers

  forCards (matchController opponent <> matchCanBlock) $ \blocker -> do
    blockerColors <-
      S.toList . view cardColors <$> requireCard blocker mempty

    forM_ finalAttackers $
      validate $ matchAttribute flying `matchOr`
                 matchAny matchProtection blockerColors

  forM_ finalAttackers $ combatDamageTo (TargetPlayer opponent) []

  malcolmTrigger <-
    foldM
      (\a cn -> (||) a <$> check (matchAttribute pirate) cn)
      False
      finalAttackers

  when malcolmTrigger (modifying malcolmCounter (+ 1))

  validateLife expectedLife opponent

-- copy of 'attackWith', but allow haste-y attacking if a different player
-- controls Frenzied Saddlebrute
attackPlayerWith :: Player -> [CardName] -> GameMonad ()
attackPlayerWith player cs = do
  saddlebruteBackdoor <- check
                           (matchInPlay <> invert (matchController player))
                           "Frenzied Saddlebrute"
  let hasteMatcher =
        if saddlebruteBackdoor then matchAll else matchAttribute haste

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

angelCounter :: Control.Lens.Lens' Board Int
angelCounter = counters . at "angels" . non 0

treasureCounter :: Control.Lens.Lens' Board Int
treasureCounter = counters . at "treasure" . non 0

malcolmCounter :: Control.Lens.Lens' Board Int
malcolmCounter = counters . at "malcolm" . non 0

allPlayers = [Active, OpponentN 1, OpponentN 2, OpponentN 3]

commander = "commander"
pirate = "pirate"

attributes = attributeFormatter $ do
  attribute "life #1" $ countLife (OpponentN 1)
  attribute "life #2" $ countLife (OpponentN 2)
  attribute "life #3" $ countLife (OpponentN 3)

playFormatter player = cardFormatter (show player <> " play (ex. lands)")
 (matchController player <> invert (matchAttribute land) <> matchZone Play)

attackFormatter = cardFormatter "attackers"
 (matchController Active <> matchAttribute attacking)

playFormatters = foldr (<>) mempty . map playFormatter

formatter step = case view stepNumber step of
  1 -> attributes <> boardFormatter
  3 -> playFormatters [Active, OpponentN 3]
  5 -> attributes <> attackFormatter <> playFormatter (OpponentN 3)
  8 -> attributes
         <> attackFormatter
         <> playFormatters [OpponentN 1, OpponentN 2, OpponentN 3]
  9 -> cardFormatter "artifacts"
         $ matchInPlay <> matchController Active <> matchAttribute artifact
  10 -> playFormatter Active <> playFormatter (OpponentN 1)
  13 -> attributes
          <> attackFormatter
          <> playFormatters [OpponentN 1, OpponentN 3]
  15 -> playFormatter Active
  14 -> mempty
  17 -> attributes
          <> attackFormatter
          <> playFormatters [OpponentN 1, OpponentN 2, OpponentN 3]
  _ -> playFormatter Active
