module Solutions.RivalsOfIxalan7 where

import Dovin.Prelude
import Dovin.V4

blocked = "blocked"

solution :: GameMonad ()
solution = do
  step "Initial state" $ do
    withZone Play $ do
      addLands 2 "Mountain"
      addLands 2 "Plains"

      withAttributes [trample] $ addCreature (5, 5) "Rowdy Crew"
      addCreature (2, 2) "Needletooth Raptor"
      addCreature (0, 1) "Tilonalli's Skinshifter"
      withAttributes [flying]
        $ addCreature (2, 2) "Tah-Crop Elite"

    withZone Hand $ do
      withAttribute haste $ addCreature (1, 1) "Fanatical Firebrand"
      addInstant "Sure Strike"

    as Opponent $ do
      setLife 7
      withZone Play $ do
        addLand "Spires of Orcaza"
        addCreature (2, 2) "Aerial Responder"
        addCreature (3, 5) "Bellowing Aegisaur 1"
        addCreature (3, 5) "Bellowing Aegisaur 2"

  step "Cast Firebrand" $ do
    tapForMana "R" "Mountain 1"
    cast "R" "Fanatical Firebrand" >> resolveTop

  step "Activate Firebrand to damage raptor, killing Aerial Responder" $ do
    activate "" "" "Fanatical Firebrand" >> resolveTop
    tap "Fanatical Firebrand"
    sacrifice "Fanatical Firebrand"
    target "Needletooth Raptor"

    trigger "" "Needletooth Raptor" >> resolveTop
    damage (const 5) (targetCard "Aerial Responder") "Needletooth Raptor"

  step "Attack with all, Skinshifter as Tah-Crop and exerting Tah-Crop" $ do
    validate (invert matchInPlay) "Aerial Responder"

    attackWith
      [ "Tah-Crop Elite"
      , "Rowdy Crew"
      , "Needletooth Raptor"
      , "Tilonalli's Skinshifter"
      ]

    exert "Tah-Crop Elite"
    forCards (matchInPlay <> matchController Active <> matchAttribute creature)
      (addEffect (effectPTAdjust (1, 1)))

    trigger "" "Tilonalli's Skinshifter" >> resolveTop
    target "Tah-Crop Elite"
    validate (matchAttribute attacking) "Tah-Crop Elite"
    addEffect (effectCopyPT "Tah-Crop Elite") "Tilonalli's Skinshifter"
    gainAttribute flying "Tilonalli's Skinshifter"

  fork "No spire, block out ground damage" $ do
    combatDamage [] "Tah-Crop Elite"
    combatDamage [] "Tilonalli's Skinshifter"
    combatDamage ["Bellowing Aegisaur 1"] "Rowdy Crew"
    combatDamage ["Bellowing Aegisaur 2"] "Needletooth Raptor"

    validateLife 0 Opponent

  fork "Spire one of the flyers, sure strike pushes through damage" $ do
    tap "Spires of Orcaza"
    untap "Tah-Crop Elite"
    loseAttribute attacking "Tah-Crop Elite"

    tapForMana "R" "Mountain 2"
    tapForMana "W" "Plains 1"
    cast "1R" "Sure Strike" >> resolveTop
    target "Tilonalli's Skinshifter"
    addEffect (effectPTAdjust (3, 0)) "Tilonalli's Skinshifter"

    combatDamage [] "Tilonalli's Skinshifter"
    combatDamage ["Bellowing Aegisaur 1"] "Rowdy Crew"
    combatDamage ["Bellowing Aegisaur 2"] "Needletooth Raptor"

    validateLife 0 Opponent


attributes = attributeFormatter $ do
  attribute "opponent life" $ countLife Opponent
  attribute "mana" $
    (+) <$> countCards
              (matchAttribute land
              <> missingAttribute tapped
              <> matchController Active
              )
        <*> countManaPool Active

formatter = oldFormatter . view stepNumber

oldFormatter 1 = attributes <> boardFormatter
oldFormatter 3 = attributes <>
  cardFormatter
    "remaining creatures"
    (matchLocation (Opponent, Play) <> matchAttribute creature)
oldFormatter 4 = attributes <>
  cardFormatter
    "non-blocked creatures"
      (matchLocation (Active, Play)
      <> matchAttribute attacking
      <> missingAttribute blocked
      )
oldFormatter 5 = attributes <>
  cardFormatter
    "damaging creatures"
    (matchLocation (Active, Play)
    <> matchAttribute attacking
    <> (missingAttribute blocked `matchOr` matchAttribute trample)
    )
oldFormatter 6 = attributes <>
  cardFormatter
    "damaging creatures"
    (matchLocation (Active, Play)
    <> matchAttribute attacking
    <> (missingAttribute blocked `matchOr` matchAttribute trample)
    )
oldFormatter _ = attributes
