module Solutions.RavnicaAllegiance7 where

import Dovin.V4

import Control.Monad (forM_)

unblockable = "unblockable"

addDeepFreeze name =
  withEffect
    matchAttached
    [ effectPTSet (0, 4)
    , effectNoAbilities
    , effectAddAbility defender
    ]
    "0/4 defender, loses other abilities"
    $ addAura name

solution :: GameMonad ()
solution = do
  step "Initial state" $ do
    as Opponent $ setLife 18

    withZone Hand $ do
      addDeepFreeze "Deep Freeze 1"
      withEffect
        matchAttached
        [ effectAddAbility defender
        , effectLoseAbility flying
        ]
        "Gain defender, lose flying"
        $ addAura "Sky Tether"
      addSorcery "Ghostform"

    withZone Play $ do
      addLands 4 "Hallowed Fountain"
      addLands 2 "Stomping Ground"
      withAttribute trample $ addCreature (0, 8) "Erratic Cyclops"
      withAttribute defender $ addCreature (0, 4) "Suspicious Bookcase 1"
      withAttribute defender $ addCreature (0, 4) "Suspicious Bookcase 2"
      addCreature (2, 1) "Goblin Locksmith"
      withAttributes [flying, vigilance] $ addCreature (3, 5) "Arcades, the Strategist"
      addDeepFreeze "Deep Freeze 2"

    as Opponent $ do
      withZone Play $ do
        withAttributes [flying, lifelink]
          $ addCreature (5, 5) "Lyra Dawnbringer"
        addCreature (1, 1) "Gyre Engineer 1"
        addCreature (1, 1) "Gyre Engineer 2"
        addCreature (1, 1) "Gyre Engineer 3"
        attach "Deep Freeze 2" "Gyre Engineer 3"

  step "Deep Freeze opposing Lyra" $ do
    tapForMana "U" "Hallowed Fountain 1"
    tapForMana "U" "Hallowed Fountain 2"
    tapForMana "G" "Stomping Ground 1"
    cast "2U" "Deep Freeze 1" >> resolveTop

    attach "Deep Freeze 1" "Lyra Dawnbringer"

  step "Sky Tether Erratic Cyclops" $ do
    tapForMana "W" "Hallowed Fountain 3"
    cast "W" "Sky Tether" >> resolveTop

    attach "Sky Tether" "Erratic Cyclops"

  step "Ghostform both bookcases" $ do
    tapForMana "U" "Hallowed Fountain 4"
    tapForMana "G" "Stomping Ground 2"

    cast "1U" "Ghostform" >> resolveTop

    forM_ [1, 2] $ \n -> do
      target $ numbered n "Suspicious Bookcase"
      gainAttribute unblockable (numbered n "Suspicious Bookcase")

  step "Attack with all, only two wizards can block Goblin and Cyclops" $ do
    -- Don't use attackWith here, because want to allow defenders to attack
    let attackers = matchInPlay <> matchAttribute creature
    forCards attackers $ \cn -> do
      tap cn
      gainAttribute attacking cn

    forCards (attackers <> matchAttribute defender) $ \cn -> do
      -- Fake Arcades effect, it shouldn't actually change their power.
      modifyCard cardStrength (\(CardStrength p t) -> CardStrength t t) cn

    -- Defenders can't block, per Goblin Locksmith
    validate (matchAttribute defender) "Gyre Engineer 3"
    validate (matchAttribute defender) "Lyra Dawnbringer"

    -- No flyers to block
    validate (matchAttribute flying) "Arcades, the Strategist"
    combatDamage [] "Arcades, the Strategist"

    -- Bookcases cannot be blocked
    validate (matchAttribute unblockable) "Suspicious Bookcase 1"
    combatDamage [] "Suspicious Bookcase 1"
    validate (matchAttribute unblockable) "Suspicious Bookcase 2"
    combatDamage [] "Suspicious Bookcase 2"

    -- Only remaining blocks

    -- Ensure creatures are threatening enough to block
    validate (matchPower 2) "Goblin Locksmith"
    validate (matchPower 8) "Erratic Cyclops"

    combatDamage ["Gyre Engineer 1"] "Goblin Locksmith"
    combatDamage ["Gyre Engineer 2"] "Erratic Cyclops"

    validateLife 0 Active

attributes = attributeFormatter $ do
  attribute "life" $ countLife Opponent

formatter _ = attributes <> boardFormatter
