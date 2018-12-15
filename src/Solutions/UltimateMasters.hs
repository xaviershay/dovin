module Solutions.UltimateMasters where

import Control.Monad

import Dovin

solution :: GameMonad ()
solution = do
  step "Initial state" $ do
    setLife Opponent 20

    withLocation (Active, Hand) $ do
      addInstant "Through the Breach"
      addSorcery "Reanimate"
      addSorcery "Vengeful Rebirth"
      addCreature (2, 5) "Stingerfling Spider"
      addCreature (15, 15) "Emrakul, the Aeons Torn"

    withLocation (Active, Play) $ do
      addPlaneswalker 3 "Liliana of the Veil"
      addArtifact "Phrexian Altar"
      addArtifact "Engineered Explosives"
      forM_ [1..8] $ \n -> addLand (numbered n "Swamp")
      forM_ [1..4] $ \n -> addLand (numbered n "Raging Ravine")

      withAttribute haste $ addCreature (4, 3) "Vengevine"
      modifyStrength (1, 1) "Vengevine" -- counter
      modifyStrength (1, 1) "Vengevine" -- Mikaeus

      addCreature (5, 5) "Mikaeus, the Unhallowed"

    withLocation (Opponent, Play) $ do
      forM_ [1..7] $ \n -> addLand (numbered n "Plains")
      forM_ [1..2] $ \n -> addLand (numbered n "Forest")

      addLand "Dark Depths"

      withAttribute flying $ do
        addCreature (4, 6) "Reya Dawnbringer"
        addCreature (4, 3) "Sublime Archangel"
        withAttribute legendary
          $ addCreature (5, 5) "Sigarda, Host of Herons"

    withLocation (Opponent, Graveyard) $ do
      addEnchantment "Bridge from Below"

  step "Discard Stingerfling to Liliana" $ do
    activatePlaneswalker 1 "Liliana of the Veil"

    discard "Stingerfling Spider"

  step "Reanimate Stingerfling, targeting Reya" $ do
    tapForMana "B" "Swamp 1"
    cast "B" "Reanimate"
    resolve "Reanimate"
    targetInLocation2 (Active, Graveyard) "Stingerfling Spider"
    returnToPlay "Stingerfling Spider"

    validate "Stingerfling Spider" $ (invert $ matchAttribute "human")
    gainAttribute undying "Stingerfling Spider"

    validate "Reya Dawnbringer" $ matchAttribute flying
    -- TODO: Add spider to stack
    target "Reya Dawnbringer"

  step "With Stingerfling on stack, sacrifice it to Phrexian Altar, exiling Bridge from Below" $ do
    activate "" "Phrexian Altar"
    sacrifice "Stingerfling Spider"
    addMana "B"
    exile "Bridge from Below"

  step "Return Stingerfling with undying from Mikaeus, target Sublime Archangel" $ do
    validate "Stingerfling Spider" $ matchAttribute undying
    returnToPlay "Stingerfling Spider"
    modifyStrength (1, 1) "Stingerfling Spider"

    validate "Sublime Archangel" $ matchAttribute flying
    -- TODO: Add spider to stack
    target "Sublime Archangel"

  step "Resolve both Stingerfling triggers, destroying two angels" $ do
    destroy "Reya Dawnbringer"
    destroy "Sublime Archangel"

  step "Sac Stingerfling for mana" $ do
    activate "" "Phrexian Altar"
    sacrifice "Stingerfling Spider"
    addMana "G"

  step "Engineered Explosives for Sigarda" $ do
    activate "2" "Engineered Explosives"
    destroy "Sigarda, Host of Herons"

  step "Through the Breach Emarkul" $ do
    forM_ [3..6] $ \n -> tapForMana "B" (numbered n "Swamp")
    tapForMana "R" "Raging Ravine 1"
    cast "4R" "Through the Breach"
    resolve "Through the Breach"
    move (Active, Hand) (Active, Play) "Emrakul, the Aeons Torn"
    modifyStrength (1, 1) "Emrakul, the Aeons Torn"
    gainAttribute undying "Emrakul, the Aeons Torn"
    gainAttribute haste "Emrakul, the Aeons Torn"

  step "Activate a raging ravine" $ do
    tapForMana "B" "Swamp 7"
    tapForMana "B" "Swamp 8"
    tapForMana "R" "Raging Ravine 2"
    tapForMana "G" "Raging Ravine 3"
    activate "2RG" "Raging Ravine 4"
    gainAttribute creature "Raging Ravine 4"
    resetStrength "Raging Ravine 4" (4, 4)
    modifyStrength (1, 1) "Raging Ravine 4"

  step "Attack with everything" $ do
    attackWith
      [ "Emrakul, the Aeons Torn"
      , "Mikaeus, the Unhallowed"
      , "Vengevine"
      , "Raging Ravine 4"
      ]

    modifyStrength (1, 1) "Raging Ravine 4"

  step "Assume opponent activates Dark Depths to block Emrakul in response to Annihilator trigger" $ do
    -- TODO: Allow acting as opponent
    -- forM_ [1..7] $ \n -> tap (numbered n "Plains")
    -- forM_ [1..2] $ \n -> tap (numbered n "Forest")
    withLocation (Opponent, Play)
      $ withAttributes [indestructible, flying, token]
      $ addCreature (20, 20) "Marit Large"

    gainAttribute "blocked" "Emrakul, the Aeons Torn"

  step "Sac Emrakul for mana, twice using undying (stacking Mikaeus trigger above shuffle graveyard one)" $ do
    activate "" "Phrexian Altar"
    sacrifice "Emrakul, the Aeons Torn"
    addMana "R"

    validate "Emrakul, the Aeons Torn" $ matchAttribute undying
    returnToPlay "Emrakul, the Aeons Torn"
    modifyStrength (1, 1) "Emrakul, the Aeons Torn"

    activate "" "Phrexian Altar"
    sacrifice "Emrakul, the Aeons Torn"
    addMana "G"

    -- Don't track deck, just remove
    forCards
      (matchLocation (Active, Graveyard))
      remove

  step "Deal damage from remaining unblocked creatures" $ do
    forCards
      (matchLocation (Active, Play) <> matchAttribute "attacking" <> missingAttribute "blocked")
      damagePlayer

  step "Sacrifice remaining creatures for mana" $ do
    -- Technically this comes back with undying, but irrelevant
    activate "" "Phrexian Altar"
    sacrifice "Raging Ravine 4"
    addMana "U"

    activate "" "Phrexian Altar"
    sacrifice "Vengevine"
    addMana "U"

    activate "" "Phrexian Altar"
    sacrifice "Mikaeus, the Unhallowed"
    addMana "U"

  step "Cast Vengeful Rebirth on Mikaeus, targeting opponent" $ do
    tapForMana "B" "Swamp 2"
    cast "4GR" "Vengeful Rebirth"
    resolve "Vengeful Rebirth"

    targetInLocation2 (Active, Graveyard) "Mikaeus, the Unhallowed"
    returnToHand "Mikaeus, the Unhallowed"
    -- TODO: Should be damage
    loseLife Opponent 6

manaAttribute = attributeFormatter $ attribute "mana" $
  (+) <$> countCards (matchAttribute "land" <> missingAttribute "tapped" <> matchController Active)
      <*> countManaPool
formatter 1 =
     cardFormatter "hand" (matchLocation (Active, Hand))
  <> cardFormatter "our creatures" (matchLocation (Active, Play))
  <> cardFormatter "opponent creatures" (matchLocation (Opponent, Play))
  <> cardFormatter "graveyard" (matchLocation (Active, Graveyard))
formatter 6 = manaAttribute <>
  cardFormatter "remaining creatures" (matchLocation (Opponent, Play) <> matchAttribute creature)
formatter 11 = manaAttribute <>
  cardFormatter "attacking creatures" (matchLocation (Active, Play) <> matchAttribute "attacking")
formatter 14 = manaAttribute <> (attributeFormatter $ attribute "life" (countLife Opponent))
formatter 16 = manaAttribute <> (attributeFormatter $ attribute "life" (countLife Opponent))
formatter _ = manaAttribute
