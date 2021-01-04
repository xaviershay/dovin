{-|
V4 adds a lot of new flexibility while better aligning with the official rules:

* Adds 'Zone' type as a better name for previous 'Location' type.
* Adds 'withZone' builder, replacing 'withLocation'.
* Adds 'matchZone' and 'matchController' matchers.
* Adds 'cardController' and 'cardZone' lens, replacing 'cardLocation'.
* Adds 'cardTargets' lens and 'withCardTarget'. Cards can now store zero of
  more targets, which can be referenced in effects and validations.
* Adds 'effectControl' and 'effectControlF', a layer 2 effect to change the
  controller of a card.
* Cards can have colors via 'withColors' and 'cardColors'.
* Cards can have protection from colors with 'effectProtection' and
  'effectProtectionF' (layer 6). This is not respected by any built-in effects
  currently, but can be used in your own effects and validations.
* Add 'check' as a non-terminal version of 'validate' that returns a boolean
  rather than throwing an error. This can be used to write dynamic solutions
  based on the state of the board.
* Remove 'CardLocation' type.
* Remove 'move'. Use one of the higher level functions instead (such as
  'moveTo')
* 'addArtifact' was no longer adds the 'enchantment' attribute to cards.
* 'OpponentN Int' constructor for 'Player' allows for multiplayer scenarios.
* 'combatDamageTo' to send damage to a different player than the default
  'Opponent'.
* Add 'attach' action of attaching equipment and aura to cards.
* Add 'matchAttached' helper for creating equipment and aura effects.
* Add 'whenMatch' helper for control flow.
-}
module Dovin.V4
  ( module Dovin.V3
  )
  where

import Dovin.V3 hiding
  ( withLocation
  , cardLocation
  , move
  , CardLocation
  , Location
  , withOwner
  )
