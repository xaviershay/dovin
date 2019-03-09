module CopySpell where

import TestPrelude

test_CopySpell = testGroup "copySpell"
  [ prove "copies a spell on the stack" $ do
      withLocation (Active, Hand) $ addInstant "Shock"
      cast "" "Shock"

      copySpell "Shock (copy)" "Shock"

      resolve "Shock (copy)"
      resolve "Shock"
  , refute
      "spell must be on stack"
      "on stack" $ do
        withLocation (Active, Hand) $ addInstant "Shock"

        copySpell "Shock (copy)" "Shock"
  ]
