module EffectCopy where

import TestPrelude.V4

test_EffectCopy = testGroup "effectCopy" $
  [ prove "it copies P/T from given card" $ do
      withZone Play $ do
        addCreature (2, 2) "Grizzly Bear"
        addCreature (0, 3) "Tilonalli's Skinshifter"
      addEffect (effectCopyPT "Grizzly Bear") "Tilonalli's Skinshifter"

      validate (matchPower 2 <> matchToughness 2) "Tilonalli's Skinshifter"
  ]
