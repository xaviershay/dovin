module Solutions.ExplorersOfIxalanContest where

import Dovin.V1
import Dovin.Prelude

-- This solution is not optimal, 130 damage is possible.
solution :: GameMonad ()
solution = do
  let goblin = "goblin"
  let pirate = "pirate"
  let lazav = "Lazav, the Multifarious"
  let shapeshift x = do
        activate "" lazav
        targetInLocation (Active, Graveyard) "Adanto Vanguard"

  let goblinToken = withLocation (Active, Play)
        . withAttributes [token, goblin]
        . addCreature (1, 1)

  step "Relevant initial state" $ do
    withLocation (Active, Hand) $ do
      withAttribute goblin $ addCreature (2, 2) "Legion Warboss"
      withAttribute doublestrike
        $ withEffect
            matchInPlay
            (matchOtherCreatures <> (const $ matchAttribute firststrike))
            (pure . setAttribute doublestrike)
        $ addCreature (2, 2) "Kwende, Pride of Femeref"

      addSorcery "Switcheroo"
      addInstant "Buccaneer's Bravado"

    withLocation (Active, Play) $ do
      addCreature (1, 3) lazav
      withAttributes [goblin, token] $ addCreature (1, 1) "Goblin 1"
      withAttributes [goblin, token] $ addCreature (1, 1) "Goblin 2"

    withLocation (Active, Graveyard) $ do
      addCreature (1, 1) "Adanto Vanguard"
      addCreature (1, 3) "Lazav, the Multifarious 2"
      withAttribute deathtouch $ addCreature (3, 3) "Isareth the Awakener"
      addCreature (4, 3) "Truefire Captain"
      withAttribute goblin $ addCreature (2, 2) "Siege-Gang Commander"
      withAttribute deathtouch $ addCreature (1, 1) "Ochran Assassin"

    withLocation (Opponent, Play) $ do
      withAttributes [indestructible, doublestrike] $
        addCreature (4, 8) "Zetalpa, Primal Dawn"
      withAttribute pirate $ addCreature (4, 4) "Angrath's Marauders"
      withAttribute firststrike $ addCreature (3, 3) "Goblin Chainwhirler"
      withEffect
        matchInPlay
        matchOtherCreatures
        (pure . setAttribute haste)
        $ addCreature (3, 3) "Garna, the Bloodflame"

  step "Cast Legion Warboss and Kwende from hand" $ do
    cast "" "Legion Warboss"
    resolve "Legion Warboss"
    cast "" "Kwende, Pride of Femeref"
    resolve "Kwende, Pride of Femeref"

  step "Buccaneer's Bravado on Angrath's, giving first strike and +1/+1" $ do
    with "Angrath's Marauders" $ \x -> do
      cast "" "Buccaneer's Bravado"
      target x
      validate x $ matchAttribute pirate
      resolve "Buccaneer's Bravado"

      modifyStrength (1, 1) x
      gainAttribute firststrike x

  step "Switcheroo Kwende and Garna, creatures get haste and doublestrike appropriately" $ do
    cast "" "Switcheroo"
    target "Kwende, Pride of Femeref"
    target "Garna, the Bloodflame"
    resolve "Switcheroo"

    move (Active, Play) (Opponent, Play) "Kwende, Pride of Femeref"
    move (Opponent, Play) (Active, Play) "Garna, the Bloodflame"

    gainAttribute summoned "Kwende, Pride of Femeref"
    gainAttribute summoned "Garna, the Bloodflame"

  step "Shapeshift to Adanto Vanguard and make indestructible" $ do
    shapeshift "Adanto Vanguard"
    activate "" lazav
    gainAttribute indestructible lazav

  step "Shapeshift Lazav to Isareth" $ do
    shapeshift "Isareth the Awakener"

  step "Begin combat, create a new goblin token from Warboss" $ do
    trigger "Legion Warboss"
    withAttribute haste $ goblinToken "Goblin 3"

  step "Attack with lazav and all goblins (haste from Garna), with mentor from Warboss" $ do
    attackWith [lazav, "Legion Warboss", "Goblin 1", "Goblin 2", "Goblin 3"]

    triggerMentor "Legion Warboss" "Goblin 1"

  step "Trigger lazav-as-isareth, reanimating Seige-Gang Commander" $ do
    trigger lazav
    targetInLocation (Active, Graveyard) "Siege-Gang Commander"
    move (Active, Graveyard) (Active, Play) "Siege-Gang Commander"

    -- haste from Garna, even though not relevant
    forM_ [4..6] $ \n -> withAttribute haste $ goblinToken (numbered n "Goblin")

  step "After declare attacker, shapeshift to Ochran Assassin, luring all enemies to block" $ do
    shapeshift "Ochran Assassin"
    loseAttribute deathtouch lazav -- From Isareth
    gainAttribute deathtouch lazav -- From Assassin

    forCards
      (matchLocation (Opponent, Play))
      (gainAttribute "blocking")
    gainAttribute "blocked" lazav

  step "After declare blockers, shapeshift to Truefire Captain" $ do
    shapeshift "Truefire Captain"
    loseAttribute deathtouch lazav -- From Assassin

  step "First strike damage from enemies to lazav, doubled from Angrath's, bounced to opponent from Truefire" $ do
    forCards
      (    matchLocation (Opponent, Play)
        <> matchAttribute "blocking"
        <> (matchAttribute firststrike `matchOr` matchAttribute doublestrike)
      )
      $ \cn -> do
        damage ((* 2) . view cardPower) (targetCard lazav) cn
        damage ((* 2) . view cardPower) (targetPlayer Opponent) cn

  step "Regular damage from enemies to lazav, doubled from Angrath's, bounced to opponent from Truefire" $ do
    forCards (matchLocation (Opponent, Play) <> matchAttribute "blocking")
      $ \cn -> do
        damage ((* 2) . view cardPower) (targetCard lazav) cn
        damage (view cardPower) (targetCard cn) lazav
        damage ((* 2) . view cardPower) (targetPlayer Opponent) cn

  step "Regular damage from attackers to player" $ do
    forCards
      (matchLocation (Active, Play) <> matchAttribute attacking <> missingAttribute "blocked")
      (combatDamage [])

  let sacrificeToSiegeGang = \name -> do
        activate "" "Siege-Gang Commander"
        validate name $ matchAttribute goblin
        sacrifice name
        damage (const 2) (targetPlayer Opponent) "Siege-Gang Commander"

  step "Siege-gang all the goblins except Siege-Gang" $ do
    commander <- requireCard "Siege-Gang Commander" mempty

    forCards
      ((invert . matchName) "Siege-Gang Commander" <> matchAttribute goblin)
      sacrificeToSiegeGang

  step "Shapeshift to Warboss, sacrifice lazav and self to Siege-Gang" $ do
    shapeshift "Legion Warboss"
    gainAttribute goblin lazav

    sacrificeToSiegeGang lazav
    sacrificeToSiegeGang "Siege-Gang Commander"

damageFormatter = attributeFormatter $ do
  attribute "cumulative damage" $ (* (-1)) <$> countLife Opponent

formatter :: Int -> Formatter
formatter 1 =
     cardFormatter "hand" (matchLocation (Active, Hand))
  <> cardFormatter "our creatures" (matchLocation (Active, Play))
  <> cardFormatter "opponent creatures" (matchLocation (Opponent, Play))
  <> cardFormatter "graveyard" (matchLocation (Active, Graveyard))
formatter 2 = cardFormatter "our creatures" (matchLocation (Active, Play))
formatter 8 = cardFormatter "attacking creatures" (matchAttribute "attacking")
formatter 9 = cardFormatter "our creatures" (matchLocation (Active, Play))
formatter 12 = damageFormatter <>
  cardFormatter
   "blocking creatures with doublestrike"
   (matchAttribute "blocking" <> matchAttribute doublestrike)
formatter 13 = damageFormatter <>
  cardFormatter "blocking creatures" (matchAttribute "blocking")
formatter 14 = damageFormatter <>
  cardFormatter
    "unblocked creatures"
    (matchAttribute "attacking" <> missingAttribute "blocked")
formatter 15 = damageFormatter <>
  cardFormatter "remaining creatures" (matchLocation (Active, Play))
formatter 16 = damageFormatter

formatter _ = blankFormatter
