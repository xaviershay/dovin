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
    tapForMana "Woodland Cemetery 1" "G"
    tapForMana "Woodland Cemetery 2" "B"
    tapForMana "Dragonskull Summit 1" "R"
    cast "1BG" "Ochran Assassin"
    resolve "Ochran Assassin"

    activate "" "Torch Courier"
    sacrifice "Torch Courier"

    gainAttribute "Ochran Assassin" "haste"

  step "Sac Whisper to get Torch Courier back" $ do
    tap "Whisper, Blood Liturgist"
    sacrifice "Whisper Agent"
    sacrifice "Whisper, Blood Liturgist"

    returnToPlay "Torch Courier"

  step "Play Rhizome Lurcher, haste with Torch Courier" $ do
    tapForMana "Woodland Cemetery 3" "G"
    tapForMana "Woodland Cemetery 4" "B"
    tapForMana "Dragonskull Summit 2" "R"
    tapForMana "Dragonskull Summit 3" "R"
    cast "2BG" "Rhizome Lurcher"
    resolve "Rhizome Lurcher"

    forCardsM
      (matchLocation (Active, Graveyard) <> matchAttribute "creature")
      (const $ modifyStrength "Rhizome Lurcher" (1, 1))

    activate "" "Torch Courier"
    sacrifice "Torch Courier"

    gainAttribute "Rhizome Lurcher" "haste"

  step "Attack with Lurcher and Assasin" $ do
    attackWith ["Rhizome Lurcher", "Ochran Assassin"]
    gainAttribute "Ochran Assassin" "blocked"

    damagePlayer "Rhizome Lurcher"

    validateLife Opponent 0
