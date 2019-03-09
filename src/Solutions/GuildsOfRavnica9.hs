module Solutions.GuildsOfRavnica9 where

import Control.Lens
import Control.Monad
import Control.Monad.Except (throwError)

import Dovin.V1

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
        triggerStorm $
          \n -> copySpell name
                  (numbered (n + 1) (zipWith const name (drop 2 name)))

        trigger "Adeliz, the Cinder Wind"
        modifyStrength (1, 1) "Adeliz, the Cinder Wind"

  -- We'll be making a lot of archers...
  let addArcherCopy name = do
        withLocation (Active, Play) $
          withAttributes [token, summoned] $ addCreature (1, 4) name

  let angel = "angel"
  let merfolk = "merfolk"

  step "Initial state" $ do
    setLife Opponent 12

    withLocation (Active, Hand) $ do
      addSorcery "Undercity Uprising"
      addSorcery "Doublecast 1"
      addInstant "Plummet 1"
      addSorcery "Quasiduplicate 1"
      withAttribute flying $ addCreature (7, 6) "Torgaar, Famine Incarnate"
      addAura "Waterknot"

    withLocation (Active, Play) $ do
      addEnchantment "Thousand-Year Storm"
      -- Has +2/+2 from Maniacal Rage aura
      withAttribute flying $ addCreature (4, 4) "Adeliz, the Cinder Wind"
      addCreature (1, 4) "Afzocan Archer"

      addLands 4 "Timber Gorge"
      addLands 4 "Submerged Boneyard"
      addLands 4 "Highland Lake"

    withLocation (Opponent, Play) $ do
      withAttribute "merfolk" $ addCreature (2, 2) "Kopala, Warden of Waves"
      withAttributes [flying, token] $ addCreature (4, 4) "Angel 1"
      withAttributes [flying, token] $ addCreature (4, 4) "Angel 2"
      withAttributes [flying, token] $ addCreature (4, 4) "Angel 3"

      withAttributes [flying, angel]
        $ withEffect
            matchInPlay
            matchOtherCreatures
            (pure . setAttribute hexproof)
        $ addCreature (3, 4) "Shalai, Voice of Plenty"

      withAttributes [flying, lifelink, angel]
        $ withEffect
            matchInPlay
            (matchOtherCreatures <> (const $ matchAttribute angel))
            (pure . over cardStrength (mkStrength (1, 1) <>) . setAttribute lifelink)
        $ addCreature (4, 4) "Lyra Dawnbringer"

      withAttribute merfolk
        $ withEffect
            matchInPlay
            (matchOtherCreatures <> (const $ matchAttribute merfolk))
            (pure . over cardStrength (mkStrength (1, 1) <>))
        $ addCreature (2, 2) "Merfolk Mistbinder 1"

      withAttribute merfolk
        $ withEffect
            matchInPlay
            (matchOtherCreatures <> (const $ matchAttribute merfolk))
            (pure . over cardStrength (mkStrength (1, 1) <>))
        $ addCreature (3, 3) "Merfolk Mistbinder 2"

  step "Use Undercity Uprising on Adeliz to destroy Shalai" $ do
    tapForMana "G" (numbered 1 "Timber Gorge")
    tapForMana "B" (numbered 1 "Submerged Boneyard")
    tapForMana "B" (numbered 2 "Submerged Boneyard")

    withTriggers (cast "1GB") "Undercity Uprising"
    resolve "Undercity Uprising"
    forCards
      (matchAttribute creature <> matchLocation (Active, Play))
      (gainAttribute deathtouch)

    with "Shalai, Voice of Plenty" $ \enemy -> do
      fight "Adeliz, the Cinder Wind" enemy
      validate enemy $ matchLocation (Opponent, Graveyard)

  step "Cast Doublecast" $ do
    tapForMana "R" (numbered 2 "Timber Gorge")
    tapForMana "R" (numbered 3 "Timber Gorge")
    withTriggers (cast "RR") "Doublecast 1"

    resolve "Doublecast 2"
    resolve "Doublecast 1"

  step "Cast Plummet to destroy all fliers" $ do
    tapForMana "G" "Timber Gorge 4"
    withTriggers (cast "G") "Plummet 1"

    -- From double doublecast earlier
    copySpell "Plummet 1" "Plummet 4"
    copySpell "Plummet 1" "Plummet 5"

    resolve "Plummet 5"
    with "Lyra Dawnbringer" $ \enemy -> do
      target enemy
      validate enemy $ matchAttribute flying
      destroy enemy
      validate enemy $ matchLocation (Opponent, Graveyard)

    forM_ [1..3] $ \n -> do
      resolve (numbered (5 - n) "Plummet")
      with (numbered n "Angel") $ \enemy -> do
        target enemy
        validate enemy $ matchAttribute flying
        destroy enemy

    resolve "Plummet 1" -- No target

  step "Quasiduplicate on archer, destroy one of the Mistbinders" $ do
    tapForMana "U" (numbered 1 "Highland Lake")
    tapForMana "U" (numbered 2 "Highland Lake")
    withTriggers (cast "UU") "Quasiduplicate 1"

    with ("Merfolk Mistbinder 2") $ \enemy -> do
      forM_ [1..4] $ \n -> do
        let tokenName = ("Afzocan Archer " <> show n)
        resolve $ numbered (5 - n) "Quasiduplicate"
        addArcherCopy tokenName
        fight tokenName enemy

      validate enemy $ matchLocation (Opponent, Graveyard)

  step "Jump-start Quasiduplicate again (w/ Waterknot), destroy merfolk" $ do
    tapForMana "U" (numbered 3 "Highland Lake")
    tapForMana "U" (numbered 4 "Highland Lake")
    withTriggers (jumpstart "UU" "Waterknot") "Quasiduplicate 1"

    with (numbered 1 "Merfolk Mistbinder") $ \enemy -> do
      forM_ [1..2] $ \n -> do
        let tokenName = ("Afzocan Archer " <> show n)
        resolve $ numbered (6 - n) "Quasiduplicate"
        addArcherCopy tokenName
        fight tokenName enemy

      validate enemy $ matchLocation (Opponent, Graveyard)

    with "Kopala, Warden of Waves" $ \enemy -> do
      forM_ [3..4] $ \n -> do
        let tokenName = numbered n "Afzocan Archer"
        resolve $ numbered (6 - n) "Quasiduplicate"
        addArcherCopy tokenName
        fight tokenName enemy

      validate enemy $ matchLocation (Opponent, Graveyard)

    forM_ [5] $ \n -> do
      let tokenName = numbered n "Afzocan Archer"
      resolve $ numbered (6 - n) "Quasiduplicate"
      addArcherCopy tokenName

  step "Torgaar, sacrificing archers to reduce cost" $ do
    tapForMana "B" (numbered 3 "Submerged Boneyard")
    tapForMana "B" (numbered 4 "Submerged Boneyard")
    sacrifice $ numbered 1 "Afzocan Archer"
    sacrifice $ numbered 2 "Afzocan Archer"
    sacrifice $ numbered 3 "Afzocan Archer"
    cast "BB" "Torgaar, Famine Incarnate"
    resolve "Torgaar, Famine Incarnate"
    setLife Opponent 10

  step "Attack with Adeliz and initial archer for lethal" $ do
    attackWith ["Adeliz, the Cinder Wind", "Afzocan Archer"]

    combatDamage [] "Adeliz, the Cinder Wind"
    combatDamage [] "Afzocan Archer"

    validateLife Opponent 0

formatter :: Int -> Formatter
formatter _ = attributeFormatter $ do
  attribute "mana" $
    countCards (matchAttribute "land" <> missingAttribute "tapped")
  attribute "storm"  $ countValue "storm"
  attribute "adeliz" $
    view cardStrength <$> requireCard "Adeliz, the Cinder Wind" mempty
  attribute "enemies" $ countCards (matchLocation (Opponent, Play))

triggerStorm :: (Int -> GameMonad ()) -> GameMonad ()
triggerStorm action = do
  maybeStorm <- use $ counters . at "storm"

  case maybeStorm of
    Nothing -> throwError "No counter in state: storm"
    Just c -> forM [1..c-1] $ \n -> action n

  return ()

