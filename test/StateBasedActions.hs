module StateBasedActions where

import TestPrelude.V2

test_SBAs = testGroup "state-based actions"
  [ prove "destroys creature with damage exceeding toughness" $ do
      withStateBasedActions $ do
        withLocation Play
          $ addCreature (1, 1) "Spirit"

        damage (const 2) (targetCard "Spirit") "Spirit"

      validate (matchLocation (Active, Graveyard)) "Spirit"
  , prove "destroys deathtouched creature" $ do
      withStateBasedActions $ do
        withLocation Play
          $ withAttribute deathtouched
          $ addCreature (1, 1) "Spirit"

      validate (matchLocation (Active, Graveyard)) "Spirit"
  , prove "does not destroy indestructible creature" $ do
      withStateBasedActions $ do
        withLocation Play
          $ withAttributes [indestructible, deathtouched]
          $ addCreature (1, 1) "Spirit"

        damage (const 2) (targetCard "Spirit") "Spirit"

      validate matchInPlay "Spirit"
   , prove "removes tokens not in play" $ do
       withStateBasedActions $ do
         withLocation Graveyard
           $ withAttribute token
           $ addArtifact "Treasure"

       validateRemoved "Treasure"
   , prove "does not remove copies on stack" $ do
       withStateBasedActions $ do
         withLocation Hand
           $ withAttribute copy
           $ addArtifact "Shock"
         cast "" "Shock"

       validate (matchLocation (Active, Stack)) "Shock"
   , prove "removes copies not on stack" $ do
       withStateBasedActions $ do
         withLocation Graveyard
           $ withAttribute copy
           $ addArtifact "Shock"

       validateRemoved "Shock"
   , prove "correctly removes damaged tokens" $ do
       withStateBasedActions $ do
         withLocation Play
           $ withAttribute token
           $ addCreature (1, 1) "Spirit"

         damage (const 1) (targetCard "Spirit") "Spirit"
       validateRemoved "Spirit"
   , prove "cancels out +1/+1 and -1/-1 counters" $ do
       withStateBasedActions $ do
         withLocation Play
           $ withPlusOneCounters 3
           $ withMinusOneCounters 2
           $ addCreature (5, 5) "Dinosaur"

       validate (matchPlusOneCounters 1 <> matchMinusOneCounters 0) "Dinosaur"
  ]
