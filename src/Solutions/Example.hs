module Solutions.Example where

import Control.Monad (forM_)

import Dovin

solution :: GameMonad ()
solution = do
  step "Initial state" $ do
    setLife Opponent 3

    addCard "Plummet" (Active, Hand) ["instant"]
    addCards 2 "Forest" (Active, Play) ["land"]
    addToken "Angel" (4, 4) (Opponent, Play) ["angel", "flying"]

  step "Plummet to destroy angel" $ do
    forM_ [1..2] $ \n -> tapForMana (numbered n "Forest") "G"
    cast "1G" "Plummet"
    resolve "Plummet"
    with "Angel" $ \enemy -> do
      target enemy
      validate enemy $ matchAttribute "flying"
      destroy enemy
