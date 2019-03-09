module Solutions.RavnicaAllegiance5 where

import Dovin.V2
import Dovin.Prelude

tapWithFreyalise mana card = do
  validate matchInPlay "Song of Freyalise"
  tapForMana mana card

-- Approach:
--   * Song of Freyalise and tapping creatures for mana
--   * Use Depths to clear first creature
--   * Use Wildgrowth Walker to gain life (which untaps Famished Paladin)
--   * Use Vona x 4 to clear opponent's board
--   * Attack with all with +1/+1 from Roar and an explore for lethal
solution :: GameMonad ()
solution = do
  step "Initial state" $ do
    setLife Opponent 16
    setLife Active 17

    withLocation Graveyard $ do
      addLand "Forest"
      addLand "Swamp"

    withLocation Hand $ do
      addEnchantment "Song of Freyalise"
      addCreature (1, 3) "Wildgrowth Walker"
      addCreature (2, 1) "Merfolk Branchwalker"
      addSorcery "Enter the Unknown"
      addInstant "Rallying Roar"
      addInstant "Depths of Desire"

    withLocation Play $ do
      addEnchantment "Fall of the Thran"
      addEnchantment "Path of Discovery"
      addCreature (3, 3) "Famished Paladin"
      withAttribute vigilance $ do
        addCreature (4, 4) "Vona, Butcher of Magan"
        addCreature (2, 3) "Tenth District Veteran"
      addCreature (2, 5) "Cacophodon"
      addPlaneswalker 3 "Jace, Ingenious Mind-Mage"

    as Opponent $ do
      withLocation Play $ do
        addCreature (3, 4) "Temple Altisaur"
        addCreature (2, 2) "Everdawn Champion"
        addCreature (2, 4) "Baird, Steward of Argive"
        addCreature (2, 1) "Dauntless Bodyguard"
        addCreature (3, 5) "Imperial Ceratops"

  step "Return lands from Fall of Thran trigger" $ do
    trigger "" "Fall of the Thran" >> resolveTop
    moveTo Play "Forest"
    moveTo Play "Swamp"

  step "Cast Song of Freyalise" $ do
    tapForMana "G" "Forest"
    tapForMana "B" "Swamp"
    cast "1G" "Song of Freyalise" >> resolveTop

  step "Use Vona to destroy Bodyguard" $ do
    activate "Destroy" "" "Vona, Butcher of Magan"
    tap "Vona, Butcher of Magan"
    loseLife Active 7

    resolveTop

    target "Dauntless Bodyguard"
    destroy "Dauntless Bodyguard"

  step "Cast Depths on Altisaur using creatures" $ do
    forCards (matchController Active <> matchInPlay <> matchAttribute creature <> missingAttribute tapped <> missingAttribute summoned) (tapWithFreyalise "U")

    cast "2U" "Depths of Desire" >> resolveTop

    as Opponent $
      moveTo Hand "Temple Altisaur"
    withLocation Play $ withAttribute token $ addArtifact "Treasure"

  step "Untap with Jace" $ do
    activatePlaneswalker "" 1 "Jace, Ingenious Mind-Mage" >> resolveTop

    forCards (matchController Active <> matchInPlay <> matchAttributes [creature, tapped]) untap

  step "Cast Wildgrowth by tapping creaturs" $ do
    tapWithFreyalise "G" "Famished Paladin"
    tapWithFreyalise "G" "Tenth District Veteran"

    cast "1G" "Wildgrowth Walker" >> resolveTop

    trigger "Explore" "Path of Discovery" >> resolveTop
    modifyCardDeprecated "Wildgrowth Walker" cardPlusOneCounters (+ 1)
    trigger "Gain life" "Wildgrowth Walker" >> resolveTop
    gainLife Active 3
    trigger "Untap" "Famished Paladin" >> resolveTop
    untap "Famished Paladin"

  step "Cast Branchwalker, floating mana from paladin untaps" $ do
    tapWithFreyalise "G" "Famished Paladin"
    tapWithFreyalise "G" "Cacophodon"

    cast "1G" "Merfolk Branchwalker" >> resolveTop

    trigger "Explore Path" "Path of Discovery"
    trigger "Explore Brachwalker" "Merfolk Branchwalker"

    resolveTop
    modifyCardDeprecated "Merfolk Branchwalker" cardPlusOneCounters (+ 1)
    trigger "Gain life" "Wildgrowth Walker" >> resolveTop
    gainLife Active 3
    trigger "Untap" "Famished Paladin" >> resolveTop
    untap "Famished Paladin"

    tapWithFreyalise "G" "Famished Paladin"

    resolveTop
    modifyCardDeprecated "Merfolk Branchwalker" cardPlusOneCounters (+ 1)
    trigger "Gain life" "Wildgrowth Walker" >> resolveTop
    gainLife Active 3
    trigger "Untap" "Famished Paladin" >> resolveTop
    untap "Famished Paladin"

    tapWithFreyalise "G" "Famished Paladin"

  step "Enter the Unknown on Tenth District Veteran" $ do
    cast "G" "Enter the Unknown" >> resolveTop
    target "Tenth District Veteran"

    trigger "Explore Path" "Path of Discovery"

    resolveTop
    modifyCardDeprecated "Tenth District Veteran" cardPlusOneCounters (+ 1)
    trigger "Gain life" "Wildgrowth Walker" >> resolveTop
    gainLife Active 3
    trigger "Untap" "Famished Paladin" >> resolveTop
    untap "Famished Paladin"

  step "Use Vona to destroy Baird" $ do
    activate "Destroy" "" "Vona, Butcher of Magan"
    tap "Vona, Butcher of Magan"
    loseLife Active 7

    resolveTop

    target "Baird, Steward of Argive"
    destroy "Baird, Steward of Argive"

  step "Float mana from creatures and artifact, untap with Rallying Roar" $ do
    forCards (matchController Active <> matchInPlay <> matchAttribute creature <> missingAttribute tapped <> missingAttribute summoned) (tapWithFreyalise "W")

    sacrifice "Treasure"
    addMana "1"

    cast "2W" "Rallying Roar" >> resolveTop

    forCards (matchController Active <> matchInPlay <> matchAttribute creature) (modifyStrength (1, 1))
    forCards (matchController Active <> matchInPlay <> matchAttributes [creature, tapped]) untap

  step "Attack with all" $ do
    attackWith
      [ "Famished Paladin"
      , "Tenth District Veteran"
      , "Vona, Butcher of Magan"
      , "Cacophodon"
      ]

  step "Use Vona to destroy Ceratops in response to Tenth District Veteran trigger" $ do
    trigger "Untap" "Tenth District Veteran"

    activate "Destroy" "" "Vona, Butcher of Magan"
    tap "Vona, Butcher of Magan"
    loseLife Active 7

    resolveTop

    target "Imperial Ceratops"
    destroy "Imperial Ceratops"


  step "Resolve veteran trigger, untap Vona" $ do
    resolve "Untap"
    untap "Vona, Butcher of Magan"

  step "Use Vona to destroy Everdawn Champion" $ do
    activate "Destroy" "" "Vona, Butcher of Magan"
    tap "Vona, Butcher of Magan"
    loseLife Active 7

    resolveTop

    target "Everdawn Champion"
    destroy "Everdawn Champion"

    validateLife 1 Active

  step "Combat damage for the win" $ do
    forCards (matchAttribute attacking) (combatDamage [])
    validateLife 0 Opponent

attributes = attributeFormatter $ do
  attribute "op. life" $ countLife Opponent
  attribute "our life" $ countLife Active
  attribute "pool" $ countManaPool Active

formatter step = attributes <> case view stepNumber step of
  1  -> boardFormatter
  4  -> cardFormatter "defending" (matchInPlay <> matchController Opponent)
  5  -> cardFormatter "defending" (matchInPlay <> matchController Opponent)
  10 -> cardFormatter "defending" (matchInPlay <> matchController Opponent)
  12 -> cardFormatter "attacking" (matchAttribute attacking)
  13 -> cardFormatter "defending" (matchInPlay <> matchController Opponent)
  15 -> cardFormatter "defending" (matchInPlay <> matchController Opponent)
  _ -> mempty
