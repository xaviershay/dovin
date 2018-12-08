-- | Attributes with special meaning to Dovin. Use these rather than strings to
-- avoid typos. They should generally match Magic keywords.
module Dovin.Attributes where

import Data.Monoid ((<>))

import Dovin.Types

-- | Return a card name suffixed by the given number.
numbered :: Int -> CardName -> CardName
numbered n name = name <> " " <> show n

aura = "aura"
aura :: CardAttribute
creature :: CardAttribute
creature = "creature"
deathtouch :: CardAttribute
deathtouch = "deathtouch"
doublestrike :: CardAttribute
doublestrike = "doublestrike"
enchantment :: CardAttribute
enchantment = "enchantment"
firststrike :: CardAttribute
firststrike = "firststrike"
flying :: CardAttribute
flying = "flying"
haste :: CardAttribute
haste = "haste"
indestructible :: CardAttribute
indestructible = "indestructible"
instant :: CardAttribute
instant = "instant"
land :: CardAttribute
land = "land"
lifelink :: CardAttribute
lifelink = "lifelink"
sorcery :: CardAttribute
sorcery = "sorcery"
summoned :: CardAttribute
summoned = "summonded"
token :: CardAttribute
token = "token"
trample :: CardAttribute
trample = "trample"
vigilance :: CardAttribute
vigilance = "vigilance"
