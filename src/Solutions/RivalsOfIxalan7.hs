module Solutions.RivalsOfIxalan7 where

import Dovin.Prelude
import Dovin

blocked = "blocked"

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
    target "Needletooth Raptor"

    trigger "Needletooth Raptor"
    damage (const 5) (targetCard "Aerial Responder") "Needletooth Raptor"

  step "Attack with all, Skinshifter as Tah-Crop and exerting Tah-Crop" $ do
    validate "Aerial Responder" $ invert matchInPlay

    attackWith
      [ "Tah-Crop Elite"
      , "Rowdy Crew"
      , "Needletooth Raptor"
      , "Tilonalli's Skinshifter"
      ]

    exert "Tah-Crop Elite"

    trigger "Tilonalli's Skinshifter"
    target "Tah-Crop Elite"
    validate "Tah-Crop Elite" $ matchAttribute attacking
    resetStrength "Tilonalli's Skinshifter" (2, 2)
    gainAttribute flying "Tilonalli's Skinshifter"

  fork
    [ step "No spire, block out ground damage" $ do
        combatDamage [] "Tah-Crop Elite"
        combatDamage [] "Tilonalli's Skinshifter"
        combatDamage ["Bellowing Aegisaur 1"] "Rowdy Crew"
        combatDamage ["Bellowing Aegisaur 2"] "Needletooth Raptor"

        validateLife Opponent 0

    , step "Spire one of the flyers, sure strike pushes through damage" $ do
        tap "Spires of Orcaza"
        untap "Tah-Crop Elite"
        loseAttribute attacking "Tah-Crop Elite"

        tapForMana "R" "Mountain 2"
        tapForMana "W" "Plains 1"
        cast "1R" "Sure Strike" >> resolveTop
        target "Tilonalli's Skinshifter"
        modifyStrength (3, 0) "Tilonalli's Skinshifter"

        combatDamage [] "Tilonalli's Skinshifter"
        combatDamage ["Bellowing Aegisaur 1"] "Rowdy Crew"
        combatDamage ["Bellowing Aegisaur 2"] "Needletooth Raptor"

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
