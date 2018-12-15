module Solutions.Example where

import Control.Monad (forM_)

import Dovin

solution :: GameMonad ()
solution = do
  step "Initial state" $ do
    setLife Opponent 3

    withLocation (Active, Hand) $ addInstant "Plummet"
    withLocation (Active, Play) $ do
      addLand (numbered 1 "Forest")
      addLand (numbered 2 "Forest")

    withLocation (Opponent, Play) $ do
      withAttributes [flying, token] $ addCreature (4, 4) "Angel"

  step "Plummet to destroy angel" $ do
    forM_ [1..2] $ \n -> tapForMana "G" (numbered n "Forest")
    cast "1G" "Plummet"
    resolve "Plummet"
    with "Angel" $ \enemy -> do
      target enemy
      validate enemy $ matchAttribute flying
      destroy enemy
