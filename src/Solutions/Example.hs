module Solutions.Example where

import Control.Lens (view)
import Control.Monad (forM_)

import Dovin

solution :: GameMonad ()
solution = do
  step "Initial state" $ do
    setLife Opponent 3

    withLocation (Active, Hand) $ addInstant "Plummet"
    withLocation (Active, Play) $ do
      addLands 2 "Forest"

    withLocation (Opponent, Play) $ do
      withAttributes [flying, token] $ addCreature (4, 4) "Angel"
      withAttributes [flying]
        $ withEffect
            matchInPlay
            (  matchOtherCreatures
            <> (const $ matchAttribute creature)
            )
            (pure . setAttribute hexproof)
        $ addCreature (3, 4) "Shalai, Voice of Plenty"

  step "Plummet to destroy Shalai" $ do
    forM_ [1..2] $ \n -> tapForMana "G" (numbered n "Forest")
    cast "1G" "Plummet"
    resolve "Plummet"
    with "Shalai, Voice of Plenty" $ \enemy -> do
      target enemy
      validate enemy $ matchAttribute flying
      destroy enemy

formatter :: Int -> Formatter
formatter 2 = manaFormatter
  <> cardFormatter "opponent creatures" (matchLocation (Opponent, Play))
formatter _ = boardFormatter

manaFormatter = attributeFormatter $ do
  attribute "availble mana" $
    countCards (matchAttribute land <> missingAttribute tapped)
