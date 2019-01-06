-- | Attributes with special meaning to Dovin. Use these rather than strings to
-- avoid typos. They should generally match Magic keywords.
module Dovin.Attributes where

import Data.Monoid ((<>))

import Dovin.Types

-- | Return a card name suffixed by the given number.
numbered :: Int -> CardName -> CardName
numbered n name = name <> " " <> show n

activated :: CardAttribute
activated = "activated"
arcane :: CardAttribute
arcane = "arcane"
attacking :: CardAttribute
attacking = "attacking"
aura :: CardAttribute
aura = "aura"
artifact :: CardAttribute
artifact = "artifact"
copy :: CardAttribute
copy = "copy"
creature :: CardAttribute
creature = "creature"
deathtouch :: CardAttribute
deathtouch = "deathtouch"
deathtouched :: CardAttribute
deathtouched = "deathtouched"
doublestrike :: CardAttribute
doublestrike = "doublestrike"
enchantment :: CardAttribute
enchantment = "enchantment"
exerted :: CardAttribute
exerted = "exerted"
exileWhenLeaveStack :: CardAttribute
exileWhenLeaveStack = "exile-when-leave-stack"
firststrike :: CardAttribute
firststrike = "firststrike"
flash :: CardAttribute
flash = "flash"
flying :: CardAttribute
flying = "flying"
haste :: CardAttribute
haste = "haste"
indestructible :: CardAttribute
indestructible = "indestructible"
instant :: CardAttribute
instant = "instant"
hexproof :: CardAttribute
hexproof = "hexproof"
land :: CardAttribute
land = "land"
legendary :: CardAttribute
legendary = "legendary"
lifelink :: CardAttribute
lifelink = "lifelink"
planeswalker :: CardAttribute
planeswalker = "planeswalker"
sorcery :: CardAttribute
sorcery = "sorcery"
storm :: CardAttribute
storm = "storm"
summoned :: CardAttribute
summoned = "summoned"
undying :: CardAttribute
undying = "undying"
tapped :: CardAttribute
tapped = "tapped"
token :: CardAttribute
token = "token"
trample :: CardAttribute
trample = "trample"
triggered :: CardAttribute
triggered = "triggered"
vigilance :: CardAttribute
vigilance = "vigilance"
