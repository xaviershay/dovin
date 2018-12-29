module Solutions.UltimateMasters where

import Control.Lens (over)
import Control.Monad

import Dovin.V2

sacrificeToAltar mana name = do
  activate "" "" "Phrexian Altar" >> resolveTop
  sacrifice name
  addMana mana

solution :: GameMonad ()
solution = do
  step "Initial state" $ do
    setLife Opponent 20

    withLocation Hand $ do
      addInstant "Through the Breach"
      withAttribute arcane $ addInstant "Goryo's Vengeance"
      addSorcery "Reanimate"
      addSorcery "Vengeful Rebirth"
      addCreature (2, 5) "Stingerfling Spider"
      addCreature (15, 15) "Emrakul, the Aeons Torn"

    withLocation Play $ do
      addPlaneswalker 3 "Liliana of the Veil"
      addArtifact "Phrexian Altar"
      addArtifact "Engineered Explosives"
      addLands 6 "Swamp"
      addLands 6 "Mountain"

      withAttribute haste
        $ withPlusOneCounters 1
        $ addCreature (4, 3) "Vengevine"

      withAttribute legendary
        $ withEffect
            matchInPlay
            (matchOtherCreatures <> (const $ matchAttributes [creature]))
            (pure . over cardStrength (mkStrength (1, 1) <>) . setAttribute undying)
        $ addCreature (5, 5) "Mikaeus, the Unhallowed"

    as Opponent $ do
      withLocation Play $ do
        addLands 7 "Plains"
        addLands 2 "Forest"

        addLand "Dark Depths"

        withAttribute flying $ do
          addCreature (4, 6) "Reya Dawnbringer"
          addCreature (4, 3) "Sublime Archangel"
          withAttribute legendary
            $ addCreature (5, 5) "Sigarda, Host of Herons"

      withLocation Graveyard $ do
        addEnchantment "Bridge from Below"

  step "Sac Vengevine and Reanimate with the created mana, exiling Bridge" $ do
    sacrificeToAltar "B" "Vengevine"

    exile "Bridge from Below"

    cast "B" "Reanimate" >> resolveTop
    targetInLocation (Active, Graveyard) "Vengevine"
    returnToPlay "Vengevine"

  step "Sac Vengevine (undying) then Mikaeus for mana" $ do
    sacrificeToAltar "U" "Vengevine"
    sacrificeToAltar "U" "Mikaeus, the Unhallowed"

  step "Engineered Explosives for Sigarda" $ do
    activate "" "2" "Engineered Explosives" >> resolveTop
    destroy "Sigarda, Host of Herons"

  step "Goryo's Mikaeus with spliced Breach of Stingerfling, destroying Archangel" $ do
    tapForMana "B" "Swamp 1"
    tapForMana "B" "Swamp 2"
    tapForMana "R" "Mountain 1"
    tapForMana "R" "Mountain 2"
    tapForMana "R" "Mountain 3"
    tapForMana "R" "Mountain 4"

    cast "1B" "Goryo's Vengeance"
    splice "Goryo's Vengeance" "2RR" "Through the Breach"
    resolveTop

    with "Mikaeus, the Unhallowed" $ \cn -> do
      targetInLocation (Active, Graveyard) cn
      validate (matchAttribute legendary) cn
      returnToPlay cn
      gainAttribute haste cn

    with "Stingerfling Spider" $ \cn -> do
      targetInLocation (Active, Hand) cn
      moveTo Play cn
      gainAttribute haste cn

      trigger "Destroy creature with flying" cn >> resolveTop
      target "Sublime Archangel"
      validate (matchAttribute flying) "Sublime Archangel"
      destroy "Sublime Archangel"

  step "Liliana to force sac of Reya" $ do
    activatePlaneswalker 1 "Liliana of the Veil"
    as Opponent $ do
      sacrifice "Reya Dawnbringer"

  step "Breach Emrakul" $ do
    tapForMana "B" "Swamp 3"
    tapForMana "B" "Swamp 4"
    tapForMana "B" "Swamp 5"
    tapForMana "R" "Mountain 5"
    tapForMana "R" "Mountain 6"
    cast "4R" "Through the Breach" >> resolveTop

    with "Emrakul, the Aeons Torn" $ \cn -> do
      targetInLocation (Active, Hand) cn
      moveTo Play cn
      gainAttribute haste cn

  step "Attack with everything" $ do
    attackWith
      [ "Emrakul, the Aeons Torn"
      , "Mikaeus, the Unhallowed"
      , "Vengevine"
      , "Stingerfling Spider"
      ]

  step "Assume opponent activates Dark Depths to block Emrakul in response to Annihilator trigger, then annihilates lands" $ do
    trigger "Annihilator 6" "Emrakul, the Aeons Torn"

    as Opponent $ do
      forM_ [1..7] $ \n -> tapForMana "W" (numbered n "Plains")
      forM_ [1..2] $ \n -> tapForMana "G" (numbered n "Forest")
      activate "" "3" "Dark Depths" >> resolveTop
      activate "" "3" "Dark Depths" >> resolveTop
      activate "" "3" "Dark Depths" >> resolveTop
      sacrifice "Dark Depths"
      withLocation Play
        $ withAttributes [indestructible, flying, token]
        $ addCreature (20, 20) "Marit Large"

    resolve "Annihilator 6"
    as Opponent $
      forM_ [1..6] $ \n -> sacrifice (numbered n "Plains")

    gainAttribute "blocked" "Emrakul, the Aeons Torn"

  step "Deal damage. Emrakul returns with undying" $ do
    forCards
      (matchLocation (Active, Play) <> matchAttribute attacking <> missingAttribute "blocked")
      (combatDamage [])

    fight "Emrakul, the Aeons Torn" "Marit Large"

  step "In second main, sacrifice remaining creatures for mana (spider twice)" $ do
    transitionTo SecondMain

    sacrificeToAltar "R" "Vengevine"
    sacrificeToAltar "G" "Stingerfling Spider"
    sacrificeToAltar "U" "Stingerfling Spider" -- From undying
    sacrificeToAltar "U" "Emrakul, the Aeons Torn"
    sacrificeToAltar "U" "Mikaeus, the Unhallowed"

  step "Cast Vengeful Rebirth on Mikaeus, targeting opponent" $ do
    tapForMana "B" "Swamp 6"
    cast "4GR" "Vengeful Rebirth" >> resolveTop

    targetInLocation (Active, Graveyard) "Mikaeus, the Unhallowed"
    returnToHand "Mikaeus, the Unhallowed"
    damage (const 6) (targetPlayer Opponent) "Vengeful Rebirth"

    validateLife 0 Opponent

manaAttribute = attributeFormatter $ attribute "mana" $
  (+) <$> countCards
            ( matchAttribute "land"
            <> missingAttribute "tapped"
            <> matchController Active
            )
      <*> countManaPool Active
formatter 1 = boardFormatter
formatter 3 = manaAttribute
  <> cardFormatter
       "creatures"
       (matchLocation (Active, Play) <> matchAttribute creature)
  <> cardFormatter
       "graveyard"
       (matchLocation (Active, Graveyard) <> matchAttribute creature)
formatter 4 = manaAttribute
  <> cardFormatter
       "remaining creatures"
       (matchLocation (Opponent, Play) <> matchAttribute creature)
formatter 5 = manaAttribute
  <> cardFormatter
       "remaining creatures"
       (matchLocation (Opponent, Play) <> matchAttribute creature)
formatter 6 = manaAttribute
  <> cardFormatter
       "remaining creatures"
       (matchLocation (Opponent, Play) <> matchAttribute creature)
formatter 8 =
  cardFormatter
    "creatures"
    (matchLocation (Active, Play) <> matchAttribute creature)
formatter 9 =
  cardFormatter
    "unblocked creatures"
    (matchLocation (Active, Play)
    <> matchAttribute creature
    <> invert (matchAttribute "blocked")
    )
formatter 10 =
     attributeFormatter (attribute "life" (countLife Opponent))
  <> cardFormatter
       "creatures"
       (matchLocation (Active, Play) <> matchAttribute creature)
formatter 11 = manaAttribute
  <> attributeFormatter (attribute "life" (countLife Opponent))
formatter 12 = manaAttribute
  <> attributeFormatter (attribute "life" (countLife Opponent))
formatter _ = manaAttribute
