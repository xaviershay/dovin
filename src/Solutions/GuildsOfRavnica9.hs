module Solutions.GuildsOfRavnica9 where

import Control.Monad

import Dovin

solution :: GameMonad ()
solution = do
  setLife Opponent 12

  -- Hand
  addCard "Undercity Uprising" (Active, Hand) ["sorcery"]
  addCard "Doublecast" (Active, Hand) ["sorcery"]
  addCard "Plummet" (Active, Hand) ["instant"]
  addCard "Quasiduplicate" (Active, Hand) ["sorcery"]
  addCreature "Torgaar, Famine Incarnate" (7, 6) (Active, Hand) ["creature"]

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
        (requireOther cn
          <> requireLocation (Opponent, Play)
          <> requireAttribute "creature")
        (attributeEffect "hexproof")

  let cn = "Lyra Dawnbringer" in
    do
      addCreature cn (5, 5) (Opponent, Play) ["angel", "flying", "lifelink"]
      addEffect cn
        (requireOther cn
          <> requireLocation (Opponent, Play)
          <> requireAttribute "angel")
        (attributeEffect "lifelink" <> strengthEffect (1, 1))

  let cn = numbered 1 "Merfolk Mistbinder" in
    do
      addCreature cn (2, 2) (Opponent, Play) ["merfolk"]
      addEffect cn
        (requireOther cn
          <> requireLocation (Opponent, Play)
          <> requireAttribute "merfolk")
        (strengthEffect (1, 1))

  let cn = numbered 2 "Merfolk Mistbinder" in
    do
      -- Has a +1/+1 counter
      addCreature cn (3, 3) (Opponent, Play) ["merfolk"]
      addEffect cn
        (requireOther cn
          <> requireLocation (Opponent, Play)
          <> requireAttribute "merfolk")
        (strengthEffect (1, 1))
  applyEffect "Shalai, Voice of Plenty"
  applyEffect "Lyra Dawnbringer"
  applyEffect "Merfolk Mistbinder 1"
  applyEffect "Merfolk Mistbinder 2"
  -- This puzzle relies heavily on casting triggers, so wrap the relevant ones
  -- up in this helper.
  let withTriggers = \action name -> do
        action name
        trigger "Thousand-Year Storm"
        storm $ const (copy name)

        trigger "Adeliz, the Cinder Wind"
        modifyStrength "Adeliz, the Cinder Wind" (1, 1)

  step "Use Undercity Uprising on Adeliz to destroy Shalai" $ do
    tap $ numbered 1 "Timber Gorge"
    tap $ numbered 1 "Submerged Boneyard"
    tap $ numbered 2 "Submerged Boneyard"

    withTriggers cast "Undercity Uprising"
    resolve "Undercity Uprising"
    forCards
      (requireAttribute "creature" <> requireLocation (Active, Play))
      (setAttribute "deathtouch")

    with "Shalai, Voice of Plenty" $ \enemy -> do
      fight "Adeliz, the Cinder Wind" enemy
      validate enemy $ requireLocation (Opponent, Graveyard)
      removeEffect enemy

  step "Cast Doublecast" $ do
    tap $ numbered 2 "Timber Gorge"
    tap $ numbered 3 "Timber Gorge"
    withTriggers cast "Doublecast"

    resolve "Doublecast"
    resolve "Doublecast"

  step "Cast Plummet to destroy all fliers" $ do
    tap "Timber Gorge 4"
    withTriggers cast "Plummet"

    -- From double doublecast earlier
    copy "Plummet"
    copy "Plummet"

    resolve "Plummet"
    with "Lyra Dawnbringer" $ \enemy -> do
      target enemy
      destroy enemy
      validate enemy $ requireLocation (Opponent, Graveyard)
      removeEffect enemy

    forM_ [1..3] $ \n -> do
      resolve "Plummet"
      with (numbered n "Angel") $ \enemy -> do
        target enemy
        destroy enemy
        validateRemoved enemy

    resolve "Plummet" -- No target

  step "Quasiduplicate on archer, destroy one of the Mistbinders" $ do
    tap $ numbered 1 "Highland Lake"
    tap $ numbered 2 "Highland Lake"
    withTriggers cast "Quasiduplicate"

    with (numbered 2 "Merfolk Mistbinder") $ \enemy -> do
      forM_ [1..4] $ \n -> do
        let tokenName = ("Afzocan Archer " <> show n)
        resolve "Quasiduplicate"
        addToken tokenName (1, 4) (Active, Play) ["summoned"]
        fight tokenName enemy

      validate enemy $ requireLocation (Opponent, Graveyard)
      removeEffect enemy

  step "Jump-start Quasiduplicate again (w/ Waterknot), destroy merfolk" $ do
    tap $ numbered 3 "Highland Lake"
    tap $ numbered 4 "Highland Lake"
    withTriggers (jumpstart "Waterknot") "Quasiduplicate"

    with (numbered 1 "Merfolk Mistbinder") $ \enemy -> do
      forM_ [1..2] $ \n -> do
        let tokenName = ("Afzocan Archer " <> show n)
        resolve "Quasiduplicate"
        addToken tokenName (1, 4) (Active, Play) ["summoned"]
        fight tokenName enemy

      validate enemy $ requireLocation (Opponent, Graveyard)
      removeEffect enemy

    with "Kopala, Warden of Waves" $ \enemy -> do
      forM_ [3..4] $ \n -> do
        let tokenName = numbered n "Afzocan Archer"
        resolve "Quasiduplicate"
        addToken tokenName (1, 4) (Active, Play) ["summoned"]
        fight tokenName enemy

      validate enemy $ requireLocation (Opponent, Graveyard)

    forM_ [5] $ \n -> do
      let tokenName = numbered n "Afzocan Archer"
      resolve "Quasiduplicate"
      addToken tokenName (1, 4) (Active, Play) ["summoned"]

  step "Torgaar, sacrificing archers to reduce cost" $ do
    tap $ numbered 3 "Submerged Boneyard"
    tap $ numbered 4 "Submerged Boneyard"
    sacrifice $ numbered 1 "Afzocan Archer"
    sacrifice $ numbered 2 "Afzocan Archer"
    sacrifice $ numbered 3 "Afzocan Archer"
    cast "Torgaar, Famine Incarnate"
    resolve "Torgaar, Famine Incarnate"
    setLife Opponent 10

  step "Attack with Adeliz and initial archer for lethal" $ do
    attackWith ["Adeliz, the Cinder Wind", "Afzocan Archer"]
