module Solutions.Example where

import Dovin

solution :: GameMonad ()
solution = do
  step "Initial state" $ do
    as Opponent $ setLife 3

    withZone Hand $ addInstant "Plummet"
    withZone Play $ do
      addLands 2 "Forest"

    as Opponent $ do
      withZone Play $ do
        withAttributes [flying, token] $ addCreature (4, 4) "Angel"
        withAttributes [flying]
          $ withEffect
              (matchOtherCreatures <$> askSelf)
              [ effectAddAbility hexproof
              ]
              "Other creatures gain hexproof"
          $ addCreature (3, 4) "Shalai, Voice of Plenty"

  step "Plummet to destroy Shalai" $ do
    tapForMana "G" (numbered 1 "Forest")
    tapForMana "G" (numbered 2 "Forest")
    cast "1G" "Plummet"
    resolve "Plummet"
    with "Shalai, Voice of Plenty" $ \enemy -> do
      target enemy
      validate (matchAttribute flying) enemy
      destroy enemy

formatter :: Step -> Formatter
formatter step = case view stepNumber step of
  1 -> manaFormatter
    <> cardFormatter "opponent creatures"
         (matchController Opponent <> matchZone Play)
  _ -> boardFormatter

manaFormatter = attributeFormatter $ do
  attribute "availble mana" $
    countCards (matchAttribute land <> missingAttribute tapped)
