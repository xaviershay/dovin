module Solutions.ForgottenRealms1 where

import Dovin.V4

import Control.Monad (forM_, when)

dragon = "dragon"

solution :: GameMonad ()
solution = do
  as Opponent $ do
    setLife 20

    withZone Play $ do
      withAttribute flying $ do
        addCreature (5, 4) "Faerie Formation"
        withAttribute token $ do
          addCreature (1, 1) "Faerie 1"
          addCreature (1, 1) "Faerie 2"

  withZone Play $ do
    addLands 2 "Forest"
    addLands 3 "Mountain"

    withPlusOneCounters 2 $ addCreature (0, 1) "Minion of the Mighty"
    addCreature (4, 3) "Leafkin Avenger"
    withEffect
      matchAttached
      [ effectPTSet (10, 10) ]
      "Base PT 10/10" $
      addArtifact "Belt of Giant Strength"

  withZone Hand $ do
    addInstant "Titanic Growth"
    addInstant "Choose Your Weapon"
    withAttributes [flying, haste, dragon] $
      addCreature (6, 6) "Inferno of the Star Mounts"

  step "Cast Titanic Growth on Minion of the Mighty" $ do
    tapForMana "G" "Forest 1"
    tapForMana "G" "Forest 2"
    cast "GG" "Titanic Growth" >> resolveTop
    target "Minion of the Mighty"
    addEffect (effectPTAdjust (4, 4)) "Minion of the Mighty"

  step "Attack with Minion, put Star Mounts into play" $ do
    attackWith ["Minion of the Mighty"]
    validate (matchPowerGTE 6) "Minion of the Mighty"
    validate (matchAttribute dragon) "Inferno of the Star Mounts"
    trigger "" "Minion of the Mighty" >> resolveTop
    moveTo Play "Inferno of the Star Mounts"
    gainAttribute attacking "Inferno of the Star Mounts"

  step "Opponent's best block" $ do
    combatDamage ["Faerie Formation", "Faerie 1"] "Minion of the Mighty"
    combatDamage ["Faerie 2"] "Inferno of the Star Mounts"

  step "Pump Inferno" $ do
    tapForMana "R" "Mountain 1"
    pumpInferno

  step "Choose your Weapon for double PT on Star Mounts using Leafkin" $ do
    tap "Leafkin Avenger"
    trigger "" "Leafkin Avenger" >> resolveTop
    forCards (matchAttribute creature <> matchZone Play <> matchController Active <> matchPowerGTE 4) $ \cn -> do
      addMana "G"
    cast "GGG" "Choose Your Weapon" >> resolveTop
    target "Inferno of the Star Mounts"
    str <- view cardStrength <$> requireCard "Inferno of the Star Mounts" mempty
    addEffect (effectPTAdjust . toTuple $ str) "Inferno of the Star Mounts"

  step "Equip Belt of Giant Strength to Inferno" $ do
    validate (matchPowerGTE 10) "Inferno of the Star Mounts"
    activate "" "" "Belt of Giant Strength"
    attach "Belt of Giant Strength" "Inferno of the Star Mounts"

  step "Pump Inferno twice to deal 20" $ do
    tapForMana "R" "Mountain 2"
    tapForMana "R" "Mountain 3"
    forM_ [1..2] $ const pumpInferno

  validateLife 0 Opponent

pumpInferno = do
  activate "Pump" "R" "Inferno of the Star Mounts" >> resolveTop
  addEffect (effectPTAdjust (1, 0)) "Inferno of the Star Mounts"

  str <- view cardPower <$> requireCard "Inferno of the Star Mounts" mempty

  when (str == 20) $
    damage (const 20) (targetPlayer Opponent) "Inferno of the Star Mounts"

formatter = const $ boardFormatter
