module Solutions.ChannelFireball where

import Dovin.V2
import Dovin.Prelude

import Data.List (delete)
import Control.Lens
import Debug.Trace

solution :: GameMonad ()
solution = do
  step "Initial state" $ do
    setLife Active 1

    withLocation Play $ do
      addLands 4 "Steam Vents"
      addLands 4 "Breeding Pool"
      addArtifact "Aetherflux Reservoir"
      addCreature (2, 2) "Beamsplitter Mage"
      addCreature (2, 2) "Man-o'-War"

    withLocation Hand $ do
      addInstant "High Tide"
      addInstant "Snap"
      addInstant "Rewind"
      addSorcery "Baral's Expertise"
      addSorcery "Channel"
      addSorcery "Fireball"
      withAttribute flash $ addCreature (2, 1) "Snapcaster Mage"
      addCreature (0, 0) "Walking Ballista"

  step "Cast High Tide with a Steam Vents" $ do
    tapForManaWithTide "U" "Steam Vents 1"
    withTriggers (cast "U") "High Tide"
    resolveAetherflux 1
    resolve "High Tide"

    modifying
      highTideCounter
      (+ 1)

  step "Cast Snapcaster, targeting High Tide, with second Steam Vents" $ do
    tapForManaWithTide "U" "Steam Vents 2"
    withTriggers (cast "1U") "Snapcaster Mage"

    resolveAetherflux 2
    resolve "Snapcaster Mage"
    targetInLocation (Active, Graveyard) "High Tide"
    gainAttribute snapped "High Tide"

  step "Flashback High Tide with third Steam Vents" $ do
    tapForManaWithTide "U" "Steam Vents 3"
    withTriggers (flashbackSnapped "U") "High Tide"

    resolveAetherflux 3
    resolve "High Tide"

    modifying
      highTideCounter
      (+ 1)

  step "Tap five remaining lands for 3 mana each" $ do
    tapForManaWithTide "R" "Steam Vents 4"
    tapForManaWithTide "G" "Breeding Pool 1"
    tapForManaWithTide "G" "Breeding Pool 2"
    tapForManaWithTide "U" "Breeding Pool 3"
    tapForManaWithTide "U" "Breeding Pool 4"

  step "Cast Walking Ballista for 1, leave on stack" $ do
    withTriggers (cast "") "Walking Ballista"

  step "Stack Snap, targeting Beamsplitter then copying to Snapcaster" $ do
    withTriggers (cast "1U") "Snap"
    target "Beamsplitter Mage"
    trigger "Copied Snap" "Beamsplitter Mage" >> resolveTop
    copySpell "Snap" "Snap Copy (Snapcaster Mage)"
    target "Snapcaster Mage"

  step "Counter Walking Ballista with Rewind, untapping Steam Vents" $ do
    withTriggers (cast "2UU") "Rewind"
    counter "Walking Ballista"
    resolveAetherflux 6
    resolve "Rewind"
    untap "Steam Vents 1"
    untap "Steam Vents 2"
    untap "Steam Vents 3"
    untap "Steam Vents 4"

  step "Resolve Snap copy, returning Snapcaster, untapping Breeding Pools" $ do
    resolve "Snap Copy (Snapcaster Mage)"

    move (Active, Play) (Active, Hand) "Snapcaster Mage"
    untap "Breeding Pool 1"
    untap "Breeding Pool 2"

  step "Resolve Snap, returning Beamsplitter, untapping Breeding Pools" $ do
    resolveAetherflux 5
    resolve "Snap"
    move (Active, Play) (Active, Hand) "Beamsplitter Mage"
    untap "Breeding Pool 3"
    untap "Breeding Pool 4"
    resolveAetherflux 4

  step "Recast Beamsplitter, stacking Snapcaster targeting Snap" $ do
    withTriggers (cast "RU") "Beamsplitter Mage"
    withTriggers (cast "1U") "Snapcaster Mage"

    resolveAetherflux 8
    resolve "Snapcaster Mage"
    resolveAetherflux 7
    resolve "Beamsplitter Mage"

    targetInLocation (Active, Graveyard) "Snap"
    gainAttribute snapped "Snap"

  step "Tap all lands for 3 mana each" $ do
    tapForManaWithTide "U" "Steam Vents 1"
    tapForManaWithTide "U" "Steam Vents 2"
    tapForManaWithTide "R" "Steam Vents 3"
    tapForManaWithTide "R" "Steam Vents 4"
    tapForManaWithTide "G" "Breeding Pool 1"
    tapForManaWithTide "G" "Breeding Pool 2"
    tapForManaWithTide "U" "Breeding Pool 3"
    tapForManaWithTide "U" "Breeding Pool 4"

  step "Stack flashbacked Snap, targeting Beamsplitter then copy to Snapcaster" $ do
    withTriggers (flashbackSnapped "1U") "Snap"
    target "Beamsplitter Mage"
    trigger "Copied Snap" "Beamsplitter Mage" >> resolveTop
    copySpell "Snap" "Snap Copy (Snapcaster Mage)"
    target "Snapcaster Mage"

  step "Resolve Snap copy, returning Snapcaster, untap Breeding Pools" $ do
    resolve "Snap Copy (Snapcaster Mage)"

    move (Active, Play) (Active, Hand) "Snapcaster Mage"
    untap "Breeding Pool 1"
    untap "Breeding Pool 2"

  step "Replay Snapcaster, targeting Rewind" $ do
    withTriggers (cast "1U") "Snapcaster Mage"
    resolveAetherflux 10
    resolve "Snapcaster Mage"
    targetInLocation (Active, Graveyard) "Rewind"
    gainAttribute snapped "Rewind"

  step "Resolve Snap on Beamsplitter, untap Steam Vents" $ do
    resolveAetherflux 9
    resolve "Snap"
    move (Active, Play) (Active, Hand) "Beamsplitter Mage"
    untap "Steam Vents 3"
    untap "Steam Vents 4"

  step "Cast Baral's Expertise for all 3 in play, replaying Aetherflux for free" $ do
    withTriggers (cast "3UU") "Baral's Expertise"
    resolveAetherflux 11
    resolve "Baral's Expertise"

    move (Active, Play) (Active, Hand) "Aetherflux Reservoir"
    move (Active, Play) (Active, Hand) "Man-o'-War"
    move (Active, Play) (Active, Hand) "Snapcaster Mage"

    withTriggers (cast "") "Aetherflux Reservoir"
    resolve "Aetherflux Reservoir"

  step "Cast Beamsplitter Mage, stacking Snapcaster targeting Baral's" $ do
    withTriggers (cast "UR") "Beamsplitter Mage"
    withTriggers (cast "1U") "Snapcaster Mage"
    resolveAetherflux 14
    resolve "Snapcaster Mage"
    resolveAetherflux 13
    resolve "Beamsplitter Mage"

    targetInLocation (Active, Graveyard) "Baral's Expertise"
    gainAttribute snapped "Baral's Expertise"

  step "Flashback Baral's Expertise targeting all 3 in play, replaying Aetherflux" $ do
    withTriggers (flashbackSnapped "3UU") "Baral's Expertise"
    resolveAetherflux 15
    resolve "Baral's Expertise"

    move (Active, Play) (Active, Hand) "Aetherflux Reservoir"
    move (Active, Play) (Active, Hand) "Beamsplitter Mage"
    move (Active, Play) (Active, Hand) "Snapcaster Mage"

    withTriggers (cast "") "Aetherflux Reservoir"
    resolve "Aetherflux Reservoir"

  step "Tap remaining lands for 3 mana each" $ do
    tapForManaWithTide "G" "Breeding Pool 1"
    tapForManaWithTide "G" "Breeding Pool 2"
    tapForManaWithTide "R" "Steam Vents 3"
    tapForManaWithTide "R" "Steam Vents 4"

  step "Cast Channel and Fireball for life only" $ do
    withTriggers (cast "GG") "Channel"
    resolveAetherflux 17
    resolve "Channel"

    l <- (-) <$> countLife Active <*> pure 1

    let payment = show l <> "R"

    loseLife Active l
    addMana $ show l

    withTriggers (cast payment) "Fireball"
    resolveAetherflux 18
    resolve "Fireball"

    damage (const l) (targetPlayer Opponent) "Fireball"

  step "Cast Beamsplitter Mage, stacking Snapcaster targeting Fireball" $ do
    withTriggers (cast "UR") "Beamsplitter Mage"
    withTriggers (cast "1U") "Snapcaster Mage"
    resolveAetherflux 20
    resolve "Snapcaster Mage"
    resolveAetherflux 19
    resolve "Beamsplitter Mage"

    targetInLocation (Active, Graveyard) "Fireball"
    gainAttribute snapped "Fireball"

  step "Cast Man-'o-War, returning Snapcaster Mage" $ do
    withTriggers (cast "2U") "Man-o'-War"
    resolveAetherflux 21
    resolve "Man-o'-War"

    target "Snapcaster Mage"
    move (Active, Play) (Active, Hand) "Snapcaster Mage"

  step "Cast Snapcaster, counter with flashback Rewind" $ do
    withTriggers (cast "1U") "Snapcaster Mage"
    withTriggers (flashbackSnapped "2UU") "Rewind"
    resolveAetherflux 23
    resolve "Rewind"
    counter "Snapcaster Mage"
    resolveAetherflux 22

    untap "Breeding Pool 1"
    untap "Breeding Pool 2"
    untap "Steam Vents 3"
    untap "Steam Vents 4"

  step "Flashback Fireball with all remaining mana and life" $ do
    tapForManaWithTide "U" "Breeding Pool 1"
    tapForManaWithTide "U" "Breeding Pool 2"
    tapForManaWithTide "R" "Steam Vents 3"
    tapForManaWithTide "R" "Steam Vents 4"

    life <- countLife Active
    mana <- countManaPool Active
    let dmg = (life - 1) + (mana - 1)
    let payment = show dmg <> "R"

    loseLife Active (life - 1)
    addMana $ show (life - 1)

    withTriggers (flashbackSnapped payment) "Fireball"
    resolveAetherflux 24
    resolve "Fireball"

    damage (const life) (targetPlayer Opponent) "Fireball"

attributes = attributeFormatter $ do
  attribute "life" $ countLife Active
  attribute "pool" $ countManaPool Active
  attribute "spells" $ use spellCounter

playExLandFormatter = cardFormatter "Play (ex. Land)"
  (matchLocation (Active, Play) <> invert (matchAttribute land))
formatter 1 = attributes <> boardFormatter
formatter 3 = attributes
  <> cardFormatter "Play" (matchLocation (Active, Play))
  <> cardFormatter "Graveyard" (matchLocation (Active, Graveyard))
formatter 6 = attributes <> stackFormatter
formatter 7 = attributes <> stackFormatter
formatter 8 = attributes <> stackFormatter
  <> cardFormatter "Play" (matchLocation (Active, Play))
formatter 9 = attributes <> stackFormatter
formatter 10 = attributes <> stackFormatter
  <> cardFormatter "Hand" (matchLocation (Active, Hand))
  <> cardFormatter "Graveyard" (matchLocation (Active, Graveyard))
formatter 11 = attributes
  <> cardFormatter "Play" (matchLocation (Active, Play))
  <> cardFormatter "Graveyard" (matchLocation (Active, Graveyard))
formatter 13 = attributes <> stackFormatter
formatter 14 = attributes <> stackFormatter
  <> cardFormatter "Hand" (matchLocation (Active, Hand))
  <> cardFormatter "Graveyard" (matchLocation (Active, Graveyard))
formatter 15 = attributes <> stackFormatter
  <> cardFormatter "Graveyard" (matchLocation (Active, Graveyard))
formatter 16 = attributes
  <> cardFormatter "Hand" (matchLocation (Active, Hand))
  <> cardFormatter "Play" (matchLocation (Active, Play))
formatter 17 = attributes
  <> cardFormatter "Hand" (matchLocation (Active, Hand))
  <> playExLandFormatter
formatter 18 = attributes
  <> playExLandFormatter
  <> cardFormatter "Graveyard" (matchLocation (Active, Graveyard))
formatter 19 = attributes
  <> cardFormatter "Hand" (matchLocation (Active, Hand))
  <> playExLandFormatter
formatter 21 = attributeFormatter (attribute "opponent" $ countLife Opponent) <> attributes
formatter 22 = attributes
  <> cardFormatter "Hand" (matchLocation (Active, Hand))
  <> playExLandFormatter
  <> cardFormatter "Graveyard" (matchLocation (Active, Graveyard))
formatter 23 = attributes
  <> cardFormatter "Hand" (matchLocation (Active, Hand))
  <> playExLandFormatter
formatter 25 = attributeFormatter (attribute "opponent" $ countLife Opponent) <> attributes
formatter _ = attributes

spellCounter :: Lens' Board Int
spellCounter = counters . at "spell-count" . non 0

highTideCounter :: Lens' Board Int
highTideCounter = counters . at "high-tide-count" . non 0

aetherfluxTriggerName n = "Aetherflux Trigger #" <> show n
snapped = "snapped"

-- Keeps track of number of spells cast, and if Aetherflux Reservoir is in play
-- triggers it.
withTriggers fn name = do
  fn name

  modifying
    spellCounter
    (+ 1)

  forCards
    (matchInPlay <> matchName "Aetherflux Reservoir")
    $ const $ do
      x <- use spellCounter
      trigger (aetherfluxTriggerName x) "Aetherflux Reservoir"

tapForManaWithTide pool cn = do
  tapForMana pool cn
  x <- use highTideCounter

  addMana (replicate x 'U')

resolveAetherflux n = do
  resolve $ aetherfluxTriggerName n

  x <- use spellCounter
  gainLife Active x

flashbackSnapped mana castName = do
  validate (matchAttribute snapped) castName
  flashback mana castName

