module Solutions.Example where

import Dovin.V2

solution :: GameMonad ()
solution = do
  step "Initial state" $ do
    setLife Opponent 3

    withLocation Hand $ addInstant "Plummet"
    withLocation Play $ do
      addLands 2 "Forest"

    as Opponent $ do
      withLocation Play $ do
        withAttributes [flying, token] $ addCreature (4, 4) "Angel"
        withAttributes [flying]
          $ withEffect
              matchInPlay
              (matchOtherCreatures <> (const $ matchAttribute creature))
              (pure . setAttribute hexproof)
          $ addCreature (3, 4) "Shalai, Voice of Plenty"

  step "Plummet to destroy Shalai" $ do
    tapForMana "G" (numbered 1 "Forest")
    tapForMana "G" (numbered 2 "Forest")
    cast "1G" "Plummet"
    resolve "Plummet"
    with "Shalai, Voice of Plenty" $ \enemy -> do
      target enemy
      validate (matchAttribute flying) enemy
      destroy enemy

formatter :: Int -> Formatter
formatter 2 = manaFormatter
  <> cardFormatter "opponent creatures" (matchLocation (Opponent, Play))
formatter _ = boardFormatter

manaFormatter = attributeFormatter $ do
  attribute "availble mana" $
    countCards (matchAttribute land <> missingAttribute tapped)
