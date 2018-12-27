module Solutions.ToughestOfTheTough where

import Dovin.Prelude
import Dovin

import qualified Data.Set as S

blocked = "blocked"

-- TODO: Document, test, move to Dovin.Actions
exerted = "exerted"

-- TODO: Document, test, move to Dovin.Actions
-- TODO: Validate: phase and attacking
exertTrigger cn = do
    gainAttribute exerted cn

solution :: GameMonad ()
solution = do
  step "Initial state" $ do
    setLife Opponent 7

    withLocation (Active, Play) $ do
      addLands 2 "Mountain"
      addLands 2 "Plains"

      withAttributes [trample] $ addCreature (5, 5) "Rowdy Crew"
      addCreature (2, 2) "Needletooth Raptor"
      addCreature (0, 1) "Tilonalli's Skinshifter"
      withAttributes [flying]
        $ withEffect
            (matchInPlay <> matchAttribute exerted)
            (    matchLocation . view cardLocation
              <> const (matchAttribute creature)
            )
            (pure . over cardStrength (mkStrength (1, 1) <>))
        $ addCreature (2, 2) "Tah-Crop Elite"

    withLocation (Active, Hand) $ do
      withAttribute haste $ addCreature (1, 1) "Fanatical Firebrand"
      addInstant "Sure Strike"

    withLocation (Opponent, Play) $ do
      addLand "Spires of Orcaza"
      addCreature (2, 2) "Aerial Responder"
      addCreature (3, 5) "Bellowing Aegisaur 1"
      addCreature (3, 5) "Bellowing Aegisaur 2"

  step "Cast Firebrand" $ do
    tapForMana "R" "Mountain 1"
    cast "R" "Fanatical Firebrand" >> resolveTop

  step "Activate Firebrand to damage raptor, killing Aerial Responder" $ do
    activate "" "Fanatical Firebrand"
    tap "Fanatical Firebrand"
    sacrifice "Fanatical Firebrand"

    -- TODO: Deal 5 damage instead of just destroying outright
    destroy "Aerial Responder"
    validate "Aerial Responder" $ matchLocation (Opponent, Graveyard)

  step "Attack with all, Skinshifter as Tah-Crop and exerting Tah-Crop" $ do
    attackWith
      [ "Tah-Crop Elite"
      , "Rowdy Crew"
      , "Needletooth Raptor"
      , "Tilonalli's Skinshifter"
      ]

    exertTrigger "Tah-Crop Elite"

    trigger "Tilonalli's Skinshifter"
    target "Tah-Crop Elite"
    validate "Tah-Crop Elite" $ matchAttribute attacking
    resetStrength "Tilonalli's Skinshifter" (2, 2)
    gainAttribute flying "Tilonalli's Skinshifter"

  fork
    [ step "No spire, block out ground damage" $ do
        gainAttribute blocked "Needletooth Raptor"
        gainAttribute blocked "Rowdy Crew"

        damagePlayer "Tah-Crop Elite"
        damagePlayer "Tilonalli's Skinshifter"
        -- TODO: Support trample properly
        modifyStrength (-5, 0) "Rowdy Crew"
        damagePlayer "Rowdy Crew"
        validateLife Opponent 0

    , step "Spire one of the flyers, sure strike pushes through damage" $ do
        gainAttribute blocked "Needletooth Raptor"
        gainAttribute blocked "Rowdy Crew"

        tap "Spires of Orcaza"
        untap "Tah-Crop Elite"
        loseAttribute attacking "Tah-Crop Elite"

        tapForMana "R" "Mountain 2"
        tapForMana "W" "Plains 1"
        cast "1R" "Sure Strike" >> resolveTop
        target "Tilonalli's Skinshifter"
        modifyStrength (3, 0) "Tilonalli's Skinshifter"
        damagePlayer "Tilonalli's Skinshifter"
        -- TODO: Support trample properly
        modifyStrength (-5, 0) "Rowdy Crew"
        damagePlayer "Rowdy Crew"
        validateLife Opponent 0

    ]


attributes = attributeFormatter $ do
  attribute "opponent life" $ countLife Opponent
  attribute "mana" $
    (+) <$> countCards
              (matchAttribute land
              <> missingAttribute tapped
              <> matchController Active
              )
        <*> countManaPool

formatter 1 = attributes <> boardFormatter
formatter 3 = attributes <>
  cardFormatter
    "remaining creatures"
    (matchLocation (Opponent, Play) <> matchAttribute creature)
formatter 4 = attributes <>
  cardFormatter
    "non-blocked creatures"
      (matchLocation (Active, Play)
      <> matchAttribute attacking
      <> missingAttribute blocked
      )
formatter 5 = attributes <>
  cardFormatter
    "damaging creatures"
    (matchLocation (Active, Play)
    <> matchAttribute attacking
    <> (missingAttribute blocked `matchOr` matchAttribute trample)
    )
formatter 6 = attributes <>
  cardFormatter
    "damaging creatures"
    (matchLocation (Active, Play)
    <> matchAttribute attacking
    <> (missingAttribute blocked `matchOr` matchAttribute trample)
    )
formatter _ = attributes
