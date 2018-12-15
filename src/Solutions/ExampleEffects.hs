module Solutions.ExampleEffects where

import Control.Monad (forM_)

import Debug.Trace
import Dovin

solution :: GameMonad ()
solution = do
  step "Initial state" $ do
    setLife Opponent 3

    withLocation (Active, Play) $ do
      with "Shalai, Voice of Plenty" $ \cn -> do
        addCreature (1, 1) "Soldier"
        withAttributes [flying, "angel"] $ addCreature (3, 4) cn

        forCards
          (matchOther cn
            <> matchLocation (Active, Play)
            <> matchAttribute creature)
          $ addEffect2 cn
              (matchName cn <> matchLocation (Active, Play))
              (attributeEffect "hexproof")

        forCards
          (matchOther cn
            <> matchLocation (Active, Play)
            <> matchAttribute creature)
          $ addEffect2 "Another"
              (matchName cn)
              (attributeEffect "hexproof")

  step "Destroy Shalai, Soldier should lose hexproof" $ do
    c <- requireCard "Soldier" mempty
    traceM . show $ c
    destroy "Shalai, Voice of Plenty"
    c <- requireCard "Soldier" mempty
    traceM . show $ c
