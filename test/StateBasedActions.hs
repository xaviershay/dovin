module StateBasedActions where

import TestPrelude

test_SBAs = testGroup "state-based actions"
  [ prove "destroys creature with damage exceeding toughness" $ do
      withStateBasedActions $ do
        withLocation (Active, Play)
          $ addCreature (1, 1) "Spirit"

        damage (const 2) (targetCard "Spirit") "Spirit"

      validate "Spirit" $ matchLocation (Active, Graveyard)
  , prove "destroys deathtouched creature" $ do
      withStateBasedActions $ do
        withLocation (Active, Play)
          $ withAttribute deathtouched
          $ addCreature (1, 1) "Spirit"

      validate "Spirit" $ matchLocation (Active, Graveyard)
  , prove "does not destroy indestructible creature" $ do
      withStateBasedActions $ do
        withLocation (Active, Play)
          $ withAttributes [indestructible, deathtouched]
          $ addCreature (1, 1) "Spirit"

        damage (const 2) (targetCard "Spirit") "Spirit"

      validate "Spirit" $ matchInPlay
   , prove "removes tokens not in play" $ do
       withStateBasedActions $ do
         withLocation (Active, Graveyard)
           $ withAttribute token
           $ addArtifact "Treasure"

       validateRemoved "Treasure"
   , prove "removes copies not on stack" $ do
       withStateBasedActions $ do
         withLocation (Active, Graveyard)
           $ withAttribute copy
           $ addArtifact "Shock"

       validateRemoved "Shock"
   , prove "correctly removes damaged tokens" $ do
       withStateBasedActions $ do
         withLocation (Active, Play)
           $ withAttribute token
           $ addCreature (1, 1) "Spirit"

         damage (const 1) (targetCard "Spirit") "Spirit"
       validateRemoved "Spirit"
  ]
