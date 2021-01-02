module Effects where

import TestPrelude.V3

test_Effects = testGroup "V3 effects" $
  [ testGroup "assorted cards"
    [ prove "Effects only apply when enabled" $ do
        withLocation Hand $ do
          withEffect
            (pure $ matchAttribute creature)
            [ effectPTSet (1, 1)
            ]
            "Creatures are 1/1" $ do
              addCreature (4, 4) "Some Creature"

          validate (matchStrength (4, 4)) "Some Creature"

    , prove "Sliver Legion" $ do
        let sliver = "sliver"
        withLocation Play $ do
          withAttribute sliver $
            withEffect
              (pure $ matchAttribute sliver <> matchInPlay)
              [ effectPTAdjustF . const $ do
                  self <- askSelf
                  slivers <- length <$> askCards (
                                             matchAttribute sliver
                                          <> matchInPlay
                                          <> matchOtherCreatures self)

                  return (slivers, slivers)
              ]
              "All Slivers get +1/+1 for each other Sliver"
                $ addCreature (7, 7) "Sliver Legion"

          validate (matchStrength (7, 7)) "Sliver Legion"

          withLocation Play $ do
            withAttribute sliver $ addCreature (2, 2) "Sliver 1"
            withAttribute sliver $ addCreature (2, 2) "Sliver 2"

          validate (matchStrength (9, 9)) "Sliver Legion"
          validate (matchStrength (4, 4)) "Sliver 1"

    , prove "Drover of the Mighty" $ do
        let dinosaur = "dinosaur"

        as Opponent $ do
          withLocation Play $ do
            withAttribute dinosaur $ addCreature (4, 4) "Dinosaur Op."

        withLocation Hand $ do
          withAttribute dinosaur $ addCreature (4, 4) "Dinosaur Hand"

        withLocation Play $ do
          withEffectWhen
            (do
              controller <- viewSelf cardController
              dinos <- askCards $ matchAttribute dinosaur
                               <> matchInPlay
                               <> matchController controller

              return $ length dinos > 0
            )
            (matchCard <$> askSelf)
            [ effectPTAdjust (2, 2)
            ]
            "+2/+2 so long as you control another dinosaur"
              $ addCreature (1, 1) "Drover of the Mighty"

        validate (matchStrength (1, 1)) "Drover of the Mighty"

        withLocation Play $ do
          withAttribute dinosaur $ addCreature (4, 4) "Dinosaur"

        validate (matchStrength (3, 3)) "Drover of the Mighty"
    ]
  -- https://blogs.magicjudges.org/ftw/l2-prep/rules-and-policy/continuous-effects/
  , testGroup "effects (magic judges examples)"
    [ prove "Humble & Godhead of Awe" $ do
        withLocation Play $ do
          addCreature (2, 2) "Grizzly Bear"
          withEffect
            (matchOtherCreatures <$> askSelf)
            [ effectPTSet (1, 1)
            ]
            "Other creatures are 1/1" $ do
              addCreature (4, 4) "Godhead of Awe"

          validate (matchStrength (1, 1)) "Grizzly Bear"

          addEffect (effectPTSet (0, 1)) "Godhead of Awe"
          addEffect (effectNoAbilities) "Godhead of Awe"

          validate (matchStrength (2, 2)) "Grizzly Bear"
    , prove "Humility & Opalescence" $ do
        withLocation Play $ do
          withCMC 4 $ withEffect
            (pure $ matchAttribute creature)
            [ effectPTSet (0, 1)
            , effectNoAbilities
            ]
            "Each creature loses all abilities and is 0/1" $
              addEnchantment "Humility"

          withCMC 4 $ withEffect
            (matchOther enchantment <$> askSelf)
            [ effectPTSetF (\c -> let cmc = view cardCmc c in return (cmc, cmc))
            , effectAddType creature
            ]
            "Other enchanments are creatures with P/T equal to CMC" $
              addEnchantment "Opalescence"

          validate (matchStrength (4, 4)) "Humility"

    , prove "Opalescence & Humility" $ do
        withLocation Play $ do
          withCMC 4 $ withEffect
            (matchOther enchantment <$> askSelf)
            [ effectPTSetF (\c -> let cmc = view cardCmc c in return (cmc, cmc))
            , effectAddType creature
            ]
            "Other enchanments are creatures with P/T equal to CMC" $
              addEnchantment "Opalescence"

          withCMC 4 $ withEffect
           (pure $ matchAttribute creature)
           [ effectPTSet (0, 1)
           , effectNoAbilities
           ]
           "Each creature loses all abilities and is 0/1" $
             addEnchantment "Humility"

          validate (matchStrength (0, 1)) "Humility"
    , prove "Conspiracy & Life and Limb" $ do
        withLocation Play $ do
          let saproling = "saproling"

          withEffect
            (pure $ matchAttribute creature)
            [ effectAddType saproling
            ]
            "Creatures are saprolings" $
              addEnchantment "Life and Limb"

          withEffect
            (pure $ matchAttribute saproling)
            [ effectAddType land
            , effectPTSet (1, 1)
            ]
            "Forests and saprolings are 1/1 saprolings and lands" $
              addEnchantment "Conspiracy"

          addCreature (2, 2) "Gutter Skulk"

          validate (matchAttribute land) "Gutter Skulk"
    ]
  ]
