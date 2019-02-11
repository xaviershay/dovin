module Target where

import TestPrelude.V2

test_Target = testGroup "target"
  [ prove "targets a card in play" $ do
      withLocation Play $ addArtifact "Mox"
      target "Mox"
  , prove "targets own card with hexproof" $ do
      withLocation Play $ withAttribute hexproof $ addArtifact "Mox"
      target "Mox"
  , refute
      "cannot target hexproof of opponent"
      "not has attribute hexproof" $ do
        as Opponent $ do
          withLocation Play $ withAttribute hexproof $ addArtifact "Mox"
        target "Mox"
  , refute
      "requires in play"
      "in play" $ do
        withLocation Graveyard $ addArtifact "Mox"
        target "Mox"
  ]
