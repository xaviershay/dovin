module AddEffect where

import TestPrelude.V3

test_AddEffect = testGroup "addEffect"
  [ prove "adding effects changes the card in line with layer system" $ do
      withLocation Play $ do
        addCreature (1, 1) "Soldier"

      addEffect (effectPTAdjust (2, 2)) "Soldier"
      addEffect (effectPTSet (3, 3)) "Soldier"

      validate (matchStrength (5, 5)) "Soldier"
  ]
