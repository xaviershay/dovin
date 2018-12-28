module Solutions.GuildsOfRavnicaPre2 where

import Dovin

solution :: GameMonad ()
solution = do
  step "Initial state" $ do
    setLife Opponent 9

    withLocation (Active, Hand) $ do
      addCreature (1, 1) "Ochran Assassin"
      addCreature (2, 2) "Rhizome Lurcher"

    withLocation (Active, Play) $ do
      addCreature (1, 1) "Torch Courier"
      withAttribute flying $ addCreature (3, 2) "Whisper Agent"
      addCreature (2, 2) "Whisper, Blood Liturgist"

      addLands 3 "Dragonskull Summit"
      addLands 4 "Woodland Cemetery"

    withLocation (Active, Graveyard) $ do
      addCreature (2, 2) "Devkarin Dissident"
      addCreature (4, 3) "Underrealm Lich"
      addCreature (3, 4) "Golgari Findbroker"
      addCreature (2, 2) "Erstwhile Trooper"
      addCreature (5, 4) "Bone Dragon"

  step "Play Ochran Assassin, haste with Torch Courier" $ do
    tapForMana "G" "Woodland Cemetery 1"
    tapForMana "B" "Woodland Cemetery 2"
    tapForMana "R" "Dragonskull Summit 1"
    cast "1BG" "Ochran Assassin"
    resolve "Ochran Assassin"

    activate "" "Torch Courier"
    sacrifice "Torch Courier"

    gainAttribute "haste" "Ochran Assassin"

  step "Sac Whisper to get Torch Courier back" $ do
    tap "Whisper, Blood Liturgist"
    sacrifice "Whisper Agent"
    sacrifice "Whisper, Blood Liturgist"

    returnToPlay "Torch Courier"

  step "Play Rhizome Lurcher, haste with Torch Courier" $ do
    tapForMana "G" "Woodland Cemetery 3"
    tapForMana "B" "Woodland Cemetery 4"
    tapForMana "R" "Dragonskull Summit 2"
    tapForMana "R" "Dragonskull Summit 3"
    cast "2BG" "Rhizome Lurcher"
    resolve "Rhizome Lurcher"

    forCards
      (matchLocation (Active, Graveyard) <> matchAttribute "creature")
      (const $ modifyStrength (1, 1) "Rhizome Lurcher")

    activate "" "Torch Courier"
    sacrifice "Torch Courier"

    gainAttribute "haste" "Rhizome Lurcher"

  step "Attack with Lurcher and Assasin, everyone blocks Assasin" $ do
    attackWith ["Rhizome Lurcher", "Ochran Assassin"]

    gainAttribute "blocked" "Ochran Assassin"

    combatDamage [] "Rhizome Lurcher"

    validateLife Opponent 0

formatter _ = boardFormatter
