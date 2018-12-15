module Solutions.GuildsOfRavnicaPre2 where

import Dovin

solution :: GameMonad ()
solution = do
  step "Initial state" $ do
    setLife Opponent 9

    addCreature "Ochran Assassin" (1, 1) (Active, Hand) []
    addCreature "Rhizome Lurcher" (2, 2) (Active, Hand) []

    addCreature "Torch Courier" (1, 1) (Active, Play) []
    addCreature "Whisper Agent" (3, 2) (Active, Play) ["flying"]
    addCreature "Whisper, Blood Liturgist" (2, 2) (Active, Play) []

    addCards 3 "Dragonskull Summit" (Active, Play) ["land"]
    addCards 4 "Woodland Cemetery" (Active, Play) ["land"]

    addCreature "Devkarin Dissident" (2, 2) (Active, Graveyard) []
    addCreature "Underrealm Lich" (4, 3) (Active, Graveyard) []
    addCreature "Golgari Findbroker" (3, 4) (Active, Graveyard) []
    addCreature "Erstwhile Trooper" (2, 2) (Active, Graveyard) []
    addCreature "Bone Dragon" (5, 4) (Active, Graveyard) []

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
      (const $ modifyStrength "Rhizome Lurcher" (1, 1))

    activate "" "Torch Courier"
    sacrifice "Torch Courier"

    gainAttribute "haste" "Rhizome Lurcher"

  step "Attack with Lurcher and Assasin" $ do
    attackWith ["Rhizome Lurcher", "Ochran Assassin"]
    gainAttribute "blocked" "Ochran Assassin"

    damagePlayer "Rhizome Lurcher"

    validateLife Opponent 0
