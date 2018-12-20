module Solutions.ExampleEffects where

import Control.Monad (forM_)
import Control.Lens (over, view)

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
      withAttributes [flying, token]
        $ withEffect
            matchInPlay
            (\card ->
                 matchLocation (view location card)
              <> matchOther (view cardName card)
              <> matchAttribute creature
            )
            (pure . over cardStrength (CardStrength 1 1 <>))
        $ addCreature (4, 4) "Angel"

      withAttributes [flying]
        $ addCreature (4, 4) "Angel 2"

formatter _ =
     cardFormatter "hand" (matchLocation (Active, Hand))
  <> cardFormatter "our creatures" (matchLocation (Active, Play))
  <> cardFormatter "opponent creatures" (matchLocation (Opponent, Play))
  <> cardFormatter "graveyard" (matchLocation (Active, Graveyard))
