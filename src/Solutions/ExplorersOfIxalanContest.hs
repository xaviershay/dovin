module Solutions.ExplorersOfIxalanContest where

import Control.Monad

import Dovin

solution :: GameMonad ()
solution = do
  let goblin = "goblin"
  let pirate = "pirate"
  let lazav = "Lazav, the Multifarious"

  step "Initial state" $ do
    withLocation (Active, Hand) $ do
      withAttribute goblin $ addCreature2 (2, 2) "Legion Warboss"
      withAttribute doublestrike $ addCreature2 (2, 2) "Kwende, Pride of Femeref"
      addSorcery "Switcheroo"
      addInstant "Buccaneer's Bravado"

    withLocation (Active, Play) $ do
      addCreature2 (1, 3) lazav
      withAttributes [goblin, token] $ addCreature2 (1, 1) "Goblin 1"
      withAttributes [goblin, token] $ addCreature2 (1, 1) "Goblin 2"

    withLocation (Active, Graveyard) $ do
      addCreature2 (1, 1) "Adanto Vanguard"
      addCreature2 (6, 4) "Vigorspore Wurm"
      addCreature2 (1, 3) "Lazav, the Multifarious 2"
      withAttribute deathtouch $ addCreature2 (3, 3) "Isareth the Awakener"
      addCreature2 (4, 3) "Truefire Captain"
      addCreature2 (2, 2) "Whisper, Blood Liturgist"
      addCreature2 (2, 2) "Siege-Gang Commander"
      withAttribute deathtouch $ addCreature2 (1, 1) "Ochran Assassin"
      withAttributes [doublestrike, vigilance, trample] $
        addCreature2 (1, 1) "Swiftblade Vindicator"

    withLocation (Opponent, Play) $ do
      withAttributes [indestructible, doublestrike] $
        addCreature2 (4, 8) "Zetalpa, Primal Dawn"
      withAttribute pirate $ addCreature2 (4, 4) "Angrath's Marauders"
      withAttribute firststrike $ addCreature2 (3, 3) "Goblin Chainwhirler"
      addCreature2 (3, 3) "Garna, the Bloodflame"

  step "Cast Legion Warboss and Kwende from hand" $ do
    cast "" "Legion Warboss"
    resolve "Legion Warboss"
    cast "" "Kwende, Pride of Femeref"
    resolve "Kwende, Pride of Femeref"

  step "Buccaneer's Bravado on Angrath's, giving first strike and +1/+1" $ do
    with "Angrath's Marauders" $ \x -> do
    cast "" "Buccaneer's Bravado"
    target x
    resolve "Buccaneer's Bravado"

    modifyStrength2 (1, 1) x
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

    forCards
      (matchLocation (Active, Play) <> matchOther "Garna, the Bloodflame")
      (gainAttribute haste)

    forCards
      (matchLocation (Opponent, Play) <> matchAttribute firststrike)
      (gainAttribute doublestrike)

  step "Shapeshift Lazav to Adanto Vanguard and make indestructible" $ do
    activate "" lazav
    targetInLocation2 (Active, Graveyard) "Adanto Vanguard"
    activate "" lazav
    gainAttribute indestructible lazav

  step "Shapeshift Lazav to Isareth" $ do
    activate "" lazav
    targetInLocation2 (Active, Graveyard) "Isareth the Awakener"

  step "Begin combat, create a new goblin token from Warboss" $ do
    trigger "Legion Warboss"
    withLocation (Active, Play) $ withAttributes [token, haste, goblin] $ addCreature2 (1, 1) "Goblin 3"

  step "Attack with lazav and all goblins (haste from Garna), with mentor from Warboss" $ do
    attackWith [lazav, "Legion Warboss", "Goblin 1", "Goblin 2", "Goblin 3"]

    mentor "Legion Warboss" "Goblin 1"

  step "Trigger lazav-as-isareth, reanimating Seige-Gang Commander" $ do
    trigger lazav
    targetInLocation2 (Active, Graveyard) "Siege-Gang Commander"
    move (Active, Graveyard) (Active, Play) "Siege-Gang Commander"

    forM_ [4..6] $ \n -> do
      withLocation (Active, Play) $ withAttributes [token, haste, goblin] $ addCreature2 (1, 1) (numbered n "Goblin")

  step "after declare attacker, shapeshift to Ochran Assassin, luring all enemies to block" $ do
    trigger lazav
    targetInLocation2 (Active, Graveyard) "Ochran Assassin"
    --removeAttribute deathtouch lazav -- From Isareth
    gainAttribute deathtouch lazav -- From Assassin

    forCards
      (matchLocation (Opponent, Play))
      (gainAttribute "blocking")
    gainAttribute "blocked" lazav

  step "after declare blockers, shapeshift to Truefire Captain" $ do
    trigger lazav
    targetInLocation2 (Active, Graveyard) "Truefire Captain"
    --removeAttribute deathtouch lazav -- From Assassin

  step "First strike damage from enemies to lazav, doubled from Angrath's, bounced to opponent from Truefire" $ do
    forCards (matchLocation (Opponent, Play) <> matchAttribute "blocking" <> (matchAttribute firststrike `matchOr` matchAttribute doublestrike))
      $ \cn -> do
        -- Just damage/fight twice rather than double damage. Not technically
        -- correct, but works for this set of triggers.
        damagePlayer cn
        damagePlayer cn
        damageCard lazav cn
        damageCard lazav cn

  step "Regular damage from enemies to lazav, doubled from Angrath's, bounced to opponent from Truefire" $ do
    forCards (matchLocation (Opponent, Play) <> matchAttribute "blocking")
      $ \cn -> do
        -- Just damage/fight twice rather than double damage. Not technically
        -- correct, but works for this set of triggers.
        damagePlayer cn
        damagePlayer cn
        damageCard lazav cn
        damageCard lazav cn
        damageCard cn lazav

  step "Regular damage from attackers to player" $ do
    forCards
      (matchLocation (Active, Play) <> matchAttribute "attacking")
      damagePlayer

  let sacrificeToSiegeGang = \name -> do
        activate "" "Siege-Gang Commander"
        sacrifice name
        -- Should be damage not lose life, but works for now
        loseLife Opponent 2

  step "Siege-gang all the goblins except Siege-Gang" $ do
    forCards
      (matchLocation (Active, Play) <> matchAttribute "goblin" <> matchOther "Siege-Gang Commander")
      sacrificeToSiegeGang

  step "Shapeshift to Warboss, sacrifice lazav and self to Siege-Gang" $ do
    activate "" lazav
    targetInLocation2 (Active, Graveyard) "Legion Warboss"

    sacrificeToSiegeGang lazav
    sacrificeToSiegeGang "Siege-Gang Commander"

formatter :: Formatter
formatter = attributeFormatter $ do
  attribute "damage" $ (* (-1)) <$> countLife Opponent
