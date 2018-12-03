module Solutions.GuildsOfRavnica9 where

import Control.Monad

import Dovin

solution :: GameMonad ()
solution = do
  -- This puzzle relies heavily on casting triggers, so wrap the relevant ones
  -- up in this helper.
  --
  -- For ease of use, spells that will be copied are named with a trailing " 1"
  -- and subsequent copies increment this number.
  let withTriggers = \action name -> do
        action name
        trigger "Thousand-Year Storm"
        storm $ \n -> copySpell name
                        (numbered (n + 1) (zipWith const name (drop 2 name)))

        trigger "Adeliz, the Cinder Wind"
        modifyStrength "Adeliz, the Cinder Wind" (1, 1)

  step "Initial state" $ do
    setLife Opponent 12

    -- Hand
    addCard "Undercity Uprising" (Active, Hand) ["sorcery"]
    addCard "Doublecast 1" (Active, Hand) ["sorcery"]
    addCard "Plummet 1" (Active, Hand) ["instant"]
    addCard "Quasiduplicate 1" (Active, Hand) ["sorcery"]
    addCreature "Torgaar, Famine Incarnate" (7, 6) (Active, Hand) ["creature"]
    addCard "Waterknot" (Active, Hand) ["aura"]

    -- Play
    addCard "Thousand-Year Storm" (Active, Play) ["enchantment"]
    -- Has +2/+2 from Maniacal Rage aura
    addCreature "Adeliz, the Cinder Wind" (4, 4) (Active, Play) ["flying"]
    addCreature "Afzocan Archer" (1, 4) (Active, Play) []
    addCards 4 "Timber Gorge" (Active, Play) ["land"]
    addCards 4 "Submerged Boneyard" (Active, Play) ["land"]
    addCards 4 "Highland Lake" (Active, Play) ["land"]

    -- Opponent
    addCreature "Kopala, Warden of Waves" (2, 2) (Opponent, Play) ["merfolk"]
    addCreature "Angel 1" (4, 4) (Opponent, Play) ["angel", "flying", "token"]
    addCreature "Angel 2" (4, 4) (Opponent, Play) ["angel", "flying", "token"]
    addCreature "Angel 3" (4, 4) (Opponent, Play) ["angel", "flying", "token"]

    let cn = "Shalai, Voice of Plenty" in
      do
        addCreature cn (3, 4) (Opponent, Play) ["angel", "flying"]
        addEffect cn
          (matchOther cn
            <> matchLocation (Opponent, Play)
            <> matchAttribute "creature")
          (attributeEffect "hexproof")

    let cn = "Lyra Dawnbringer" in
      do
        addCreature cn (5, 5) (Opponent, Play) ["angel", "flying", "lifelink"]
        addEffect cn
          (matchOther cn
            <> matchLocation (Opponent, Play)
            <> matchAttribute "angel")
          (attributeEffect "lifelink" <> strengthEffect (1, 1))

    let cn = numbered 1 "Merfolk Mistbinder" in
      do
        addCreature cn (2, 2) (Opponent, Play) ["merfolk"]
        addEffect cn
          (matchOther cn
            <> matchLocation (Opponent, Play)
            <> matchAttribute "merfolk")
          (strengthEffect (1, 1))

    let cn = numbered 2 "Merfolk Mistbinder" in
      do
        -- Has a +1/+1 counter
        addCreature cn (3, 3) (Opponent, Play) ["merfolk"]
        addEffect cn
          (matchOther cn
            <> matchLocation (Opponent, Play)
            <> matchAttribute "merfolk")
          (strengthEffect (1, 1))
    applyEffect "Shalai, Voice of Plenty"
    applyEffect "Lyra Dawnbringer"
    applyEffect "Merfolk Mistbinder 1"
    applyEffect "Merfolk Mistbinder 2"

  step "Use Undercity Uprising on Adeliz to destroy Shalai" $ do
    tapForMana (numbered 1 "Timber Gorge") "G"
    tapForMana (numbered 1 "Submerged Boneyard") "B"
    tapForMana (numbered 2 "Submerged Boneyard") "B"

    withTriggers (cast "1GB") "Undercity Uprising"
    resolve "Undercity Uprising"
    forCards
      (matchAttribute "creature" <> matchLocation (Active, Play))
      (setAttribute "deathtouch")

    with "Shalai, Voice of Plenty" $ \enemy -> do
      fight "Adeliz, the Cinder Wind" enemy
      validate enemy $ matchLocation (Opponent, Graveyard)
      removeEffect enemy

  step "Cast Doublecast" $ do
    tapForMana (numbered 2 "Timber Gorge") "R"
    tapForMana (numbered 3 "Timber Gorge") "R"
    withTriggers (cast "RR") "Doublecast 1"

    resolve "Doublecast 2"
    resolve "Doublecast 1"

  step "Cast Plummet to destroy all fliers" $ do
    tapForMana "Timber Gorge 4" "G"
    withTriggers (cast "G") "Plummet 1"

    -- From double doublecast earlier
    copySpell "Plummet 1" "Plummet 4"
    copySpell "Plummet 1" "Plummet 5"

    resolve "Plummet 5"
    with "Lyra Dawnbringer" $ \enemy -> do
      target enemy
      validate enemy $ matchAttribute "flying"
      destroy enemy
      validate enemy $ matchLocation (Opponent, Graveyard)
      removeEffect enemy

    forM_ [1..3] $ \n -> do
      resolve (numbered (5 - n) "Plummet")
      with (numbered n "Angel") $ \enemy -> do
        target enemy
        validate enemy $ matchAttribute "flying"
        destroy enemy
        validateRemoved enemy

    resolve "Plummet 1" -- No target

  step "Quasiduplicate on archer, destroy one of the Mistbinders" $ do
    tapForMana (numbered 1 "Highland Lake") "U"
    tapForMana (numbered 2 "Highland Lake") "U"
    withTriggers (cast "UU") "Quasiduplicate 1"

    with (numbered 2 "Merfolk Mistbinder") $ \enemy -> do
      forM_ [1..4] $ \n -> do
        let tokenName = ("Afzocan Archer " <> show n)
        resolve $ numbered (5 - n) "Quasiduplicate"
        addToken tokenName (1, 4) (Active, Play) ["summoned"]
        fight tokenName enemy

      validate enemy $ matchLocation (Opponent, Graveyard)
      removeEffect enemy

  step "Jump-start Quasiduplicate again (w/ Waterknot), destroy merfolk" $ do
    tapForMana (numbered 3 "Highland Lake") "U"
    tapForMana (numbered 4 "Highland Lake") "U"
    withTriggers (jumpstart "UU" "Waterknot") "Quasiduplicate 1"

    with (numbered 1 "Merfolk Mistbinder") $ \enemy -> do
      forM_ [1..2] $ \n -> do
        let tokenName = ("Afzocan Archer " <> show n)
        resolve $ numbered (6 - n) "Quasiduplicate"
        addToken tokenName (1, 4) (Active, Play) ["summoned"]
        fight tokenName enemy

      validate enemy $ matchLocation (Opponent, Graveyard)
      removeEffect enemy

    with "Kopala, Warden of Waves" $ \enemy -> do
      forM_ [3..4] $ \n -> do
        let tokenName = numbered n "Afzocan Archer"
        resolve $ numbered (6 - n) "Quasiduplicate"
        addToken tokenName (1, 4) (Active, Play) ["summoned"]
        fight tokenName enemy

      validate enemy $ matchLocation (Opponent, Graveyard)

    forM_ [5] $ \n -> do
      let tokenName = numbered n "Afzocan Archer"
      resolve $ numbered (6 - n) "Quasiduplicate"
      addToken tokenName (1, 4) (Active, Play) ["summoned"]

  step "Torgaar, sacrificing archers to reduce cost" $ do
    tapForMana (numbered 3 "Submerged Boneyard") "B"
    tapForMana (numbered 4 "Submerged Boneyard") "B"
    sacrifice $ numbered 1 "Afzocan Archer"
    sacrifice $ numbered 2 "Afzocan Archer"
    sacrifice $ numbered 3 "Afzocan Archer"
    cast "BB" "Torgaar, Famine Incarnate"
    resolve "Torgaar, Famine Incarnate"
    setLife Opponent 10

  step "Attack with Adeliz and initial archer for lethal" $ do
    attackWith ["Adeliz, the Cinder Wind", "Afzocan Archer"]
    damagePlayer "Adeliz, the Cinder Wind"
    damagePlayer "Afzocan Archer"

    validateLife Opponent 0
