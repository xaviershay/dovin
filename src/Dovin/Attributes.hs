-- | Attributes with special meaning to Dovin. Use these rather than strings to
-- avoid typos. They should generally match Magic keywords.
module Dovin.Attributes where

import Data.Monoid ((<>))

import Dovin.Types

-- | Return a card name suffixed by the given number.
numbered :: Int -> CardName -> CardName
numbered n name = name <> " " <> show n

aura :: CardAttribute
aura = "aura"
creature :: CardAttribute
creature = "creature"
deathtouch :: CardAttribute
deathtouch = "deathtouch"
enchantment :: CardAttribute
enchantment = "enchantment"
flying :: CardAttribute
flying = "flying"
instant :: CardAttribute
instant = "instant"
land :: CardAttribute
land = "land"
lifelink :: CardAttribute
lifelink = "lifelink"
summoned :: CardAttribute
summoned = "summonded"
sorcery :: CardAttribute
sorcery = "sorcery"
token :: CardAttribute
token = "token"
