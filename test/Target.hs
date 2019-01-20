module Target where

import TestPrelude.V2

test_Target = testGroup "target"
  [ prove "targets a card in play" $ do
      withLocation Play $ addArtifact "Mox"
      target "Mox"
  , refute
      "cannot target hexproof"
      "not has attribute hexproof" $ do
        withLocation Play $ withAttribute hexproof $ addArtifact "Mox"
        target "Mox"
  , refute
      "requires in play"
      "in play" $ do
        withLocation Graveyard $ addArtifact "Mox"
        target "Mox"
  ]
