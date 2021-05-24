module Solutions.Strixhaven2 where

import Control.Monad (forM_)
import Dovin


import Dovin.Types
import Control.Lens (at, non, modifying)

garruk = "garruk"

solution :: GameMonad ()
solution = do
  step "Initial state" $ do
    as Active $ do
      setLife 16
      withZone Deck $ do
        forM_ [1..36] $ \n -> do
          addCard ("Unknown " <> show n)

      withZone Play $ do
        addLands 4 "Necroblossom Snarl"
        addLands 3 "Vineglimmer Snarl"

        withAttribute token $ addCreature (1, 1) "Wolf 1"
        withAttribute token $ addCreature (1, 1) "Wolf 2"

        addPlaneswalker 4 "Kasmina, Enigma Sage"
        withAttribute garruk $ addPlaneswalker 5 "Garruk, Cursed Huntsman"
        withAttribute flying $ addCreature (1, 1) "Eyetwitch"

        withPlusOneCounters 6 $ addCreature (1, 1) "Gingerbrute"

      withZone Hand $ do
        addInstant "Plumb the Forbidden"
        addSorcery "Peer into the Abyss"
        addInstant "Quandrix Command"

      withZone Exile $ do
        addInstant "Mystic Reflection"
    as Opponent $ do
      setLife 19

      withZone Play $ do
        -- Not adding all opponent creatures because they are extraneous to
        -- solution. Just leaving a potential blocker here to ensure is dealth
        -- with.
        withAttribute lifelink $ addCreature (3, 5) "Cubwarden"

  step "Cast Mystic Reflection targeting Wolf" $ do
    tapForMana "U" "Vineglimmer Snarl 1"
    castFromZone Exile "U" "Mystic Reflection" >> resolveTop
    target "Wolf 1"

  step "-1 Kasmina for Wolf token with +1/+1 counter" $ do
    activatePlaneswalker "Make token" (-1) "Kasmina, Enigma Sage" >> resolveTop

    -- Wolf because of Mystic Reflection
    withAttribute token $ withPlusOneCounters 1 $
      addCreature (1, 1) "Wolf 3"

  step "Plumb the Forbidden sacrificing wolves and Eyetwitch" $ do
    -- NOTE: Ignoring Lorescale triggers, not relevant

    tapForMana "B" "Necroblossom Snarl 1"
    tapForMana "B" "Necroblossom Snarl 2"
    cast "1B" "Plumb the Forbidden"
    sacrifice "Eyetwitch"
    trigger "Learn" "Eyetwitch"
    copySpell ("Plumb the Forbidden Eyetwitch") "Plumb the Forbidden"

    let wolves = ["Wolf 1", "Wolf 2", "Wolf 3"]

    forM_ wolves $ \cn -> do
      sacrifice cn
      trigger ("Garruk " <> cn) cn
      copySpell ("Plumb the Forbidden " <> cn) "Plumb the Forbidden"

    forM_ (reverse wolves) $ \cn -> do
      resolve ("Plumb the Forbidden " <> cn)
      loseLife 1
      draw 1
      resolve ("Garruk " <> cn) 
      forCards (matchAttribute garruk) $
        modifyCard cardLoyalty (+ 1)

    resolve "Plumb the Forbidden Eyetwitch"
    loseLife 1
    draw 1

    resolve "Learn"
    discard "Peer into the Abyss"
    draw 1

    resolve "Plumb the Forbidden"
    loseLife 1
    draw 1
  step "Cast Quandrix Command" $ do
    tapForMana "G" "Necroblossom Snarl 3"
    tapForMana "U" "Vineglimmer Snarl 2"
    tapForMana "U" "Vineglimmer Snarl 3"
    cast "1GU" "Quandrix Command" >> resolveTop

    target "Gingerbrute"
    modifyCard cardPlusOneCounters (+ 2) "Gingerbrute"

    targetInZone Graveyard "Peer into the Abyss"

    shuffleIn "Peer into the Abyss"

  step "Kasmina ultimate via Garrruk" $ do
    validate (matchInPlay <> matchController Active) "Kasmina, Enigma Sage"
    activatePlaneswalker "Ult" (-8) "Garruk, Cursed Huntsman" >> resolveTop

    exile "Peer into the Abyss"
    castFromZone Exile "" "Peer into the Abyss" >> resolveTop

    as Opponent $ do
      validateLife 19 Opponent
      loseLife $ ceiling (19 / 2)

  step "Attack with Gingerbrute" $ do
    tapForMana "B" "Necroblossom Snarl 4"
    activate "" "1" "Gingerbrute" >> resolveTop
    attackWith ["Gingerbrute"]
    forCards (matchInPlay <> matchController Opponent) $
      validate (missingAttribute haste)
    combatDamage [] "Gingerbrute"
    validateLife 0 Opponent


-- TODO: Promote to Actions
shuffleIn cn = do
  actor <- view envActor
  modifyCard cardZone (const Deck) cn
  modifying
    (deck . at actor . non [])
    ((:) cn)

formatter :: Step -> Formatter
formatter step = case view stepNumber step of
  1 -> manaFormatter
    <> cardFormatter "opponent creatures"
         (matchController Opponent <> matchZone Play)
  _ -> boardFormatter

manaFormatter = attributeFormatter $ do
  attribute "availble mana" $
    countCards (matchAttribute land <> missingAttribute tapped)
