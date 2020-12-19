{-# LANGUAGE ScopedTypeVariables #-}

module Dovin.Helpers where

import Dovin.Types
import Dovin.Attributes
import Dovin.Prelude

import Data.List (sort)
import qualified Data.HashMap.Strict as M
import qualified Data.Set as S
import Data.Char (isDigit)
import Control.Lens (_1, _2, ASetter, both, _Just)
import Control.Monad.State (get)
import Control.Monad.Reader (ask)

import Text.Parsec

applyMatcherWithDesc :: CardMatcher -> Card -> Either String ()
applyMatcherWithDesc (CardMatcher d f) c =
  if f c then
    Right ()
  else
    Left d

hasAttribute attr = S.member attr . view cardAttributes

manaSpec = mconcat <$> many (colorless <|> colored)
  where
    colorless = do
      n <- read <$> many1 digit

      return $ replicate n 'X'
    colored = many1 (oneOf "RUGBW")

parseMana :: String -> ManaPool
-- sort puts the Xs at the back
parseMana pool =
  case parse manaSpec "mana" pool of
    Left err -> mempty
    Right x -> sort x

requireCard :: CardName -> CardMatcher -> GameMonad Card
requireCard name f = do
  maybeCard <- use $ resolvedCards . at name

  case maybeCard of
    Nothing -> throwError $ "Card does not exist: " <> name
    Just card ->
      case applyMatcherWithDesc f card of
        Right () -> return card
        Left msg ->
          throwError $ name <> " does not match requirements: " <> msg

resolveEffects :: GameMonad ()
resolveEffects = do
  board <- get
  cs <- resolveEffects' board
  assign resolvedCards cs

resolveEffects' :: Board -> GameMonad (M.HashMap CardName Card)
resolveEffects' board = foldM f mempty (M.toList $ view cards board)
  where
    f a (cn, c) = do
                    card <- applyEffects c
                    return (M.insert cn card a)

    applyEffects :: BaseCard -> GameMonad Card
    applyEffects (BaseCard card) = do
      cs <- map unwrap . M.elems <$> use cards

      let allEffects =
            concatMap
              (\c -> map (\e -> (e, c)) . view cardEffects $ c)
              cs

      let enabledEffects =
            filter
              (\(e, c) -> applyMatcher (view effectEnabled e) c)
              allEffects

      let applicableEffects =
            filter
              (\(e, c) -> applyMatcher (view effectFilter e c) card)
              enabledEffects

      card' <- foldM (\c (e, _) -> applyEffect2 c e) card applicableEffects

      let plusModifier = let n = view cardPlusOneCounters card' in
                              mkStrength (n, n)
      let minusModifier = let n = view cardMinusOneCounters card' in
                              mkStrength (-n, -n)

      let strengthModifier = view cardStrengthModifier card'

      return
        $ over
            cardStrength
            ((strengthModifier <> plusModifier <> minusModifier) <>)
            card'

      where
        applyEffect2 :: Card -> CardEffect -> GameMonad Card
        applyEffect2 card e = view effectAction e card

        unwrap :: BaseCard -> Card
        unwrap (BaseCard card) = card

allCards :: GameMonad [Card]
allCards = M.elems <$> use resolvedCards

modifyCardDeprecated :: CardName -> ASetter Card Card a b -> (a -> b) -> GameMonad ()
modifyCardDeprecated name lens f = do
  modifying
    (cards . at name . _Just)
    (\(BaseCard c) -> BaseCard $ over lens f c)

  card <- requireCard name mempty

  -- This isn't a SBA, it needs to be post-condition here to make sure no funny
  -- business is happening.
  when (view cardPlusOneCounters card < 0) $
    throwError "Cannot reduce +1/+1 counters to less than 0"

  resolveEffects

modifyCard :: ASetter Card Card a b -> (a -> b) -> CardName -> GameMonad ()
modifyCard lens f name = modifyCardDeprecated name lens f

-- CARD MATCHERS
--
-- Matchers are used for both filtering sets of cards, and also for verifying
-- attributes of cards.
--
-- A wrapping type is used since I intend to add labels/introspection
-- capabilities at some point.
matchDamage :: Int -> CardMatcher
matchDamage n = CardMatcher (show n <> " damage") $
  (==) n . view cardDamage

matchLoyalty :: Int -> CardMatcher
matchLoyalty n = CardMatcher (show n <> " loyalty") $
  (==) n . view cardLoyalty

matchPlusOneCounters :: Int -> CardMatcher
matchPlusOneCounters n = CardMatcher (show n <> " +1/+1 counters") $
  (==) n . view cardPlusOneCounters

matchMinusOneCounters :: Int -> CardMatcher
matchMinusOneCounters n = CardMatcher (show n <> " -1/-1 counters") $
  (==) n . view cardMinusOneCounters

matchLocation :: CardLocation -> CardMatcher
matchLocation loc = CardMatcher ("in location " <> show loc) $
  (==) loc . view cardLocation

matchInPlay = CardMatcher "in play" $ \c -> snd (view location c) == Play

matchAttribute :: CardAttribute -> CardMatcher
matchAttribute attr = CardMatcher ("has attribute " <> attr) $
  S.member attr . view cardAttributes

matchAttributes :: [CardAttribute] -> CardMatcher
matchAttributes = foldr ((<>) . matchAttribute) mempty

matchName :: CardName -> CardMatcher
matchName n = CardMatcher ("has name " <> n) $ (==) n . view cardName

matchOtherCreatures :: Card -> CardMatcher
matchOtherCreatures = matchOther creature

matchOther :: CardAttribute -> Card -> CardMatcher
matchOther attribute card =
     matchLocation (view cardLocation card)
  <> matchAttribute attribute
  <> invert (matchName (view cardName card))

matchController player = CardMatcher ("has controller " <> show player) $
  (==) player . view (location . _1)

matchLesserPower n = CardMatcher ("power < " <> show n) $
  (< n) . view cardPower

matchToughness :: Int -> CardMatcher
matchToughness n = labelMatch ("toughness = " <> show n) $ CardMatcher ""
  ((== n) . view cardToughness) <> matchAttribute creature

matchTarget :: Target -> CardMatcher
matchTarget t = labelMatch ("target = " <> show t) $ CardMatcher ""
  ((==) t . TargetCard . view cardName)

missingAttribute = invert . matchAttribute

(CardMatcher d1 f) `matchOr` (CardMatcher d2 g) =
    CardMatcher (d1 <> " or " <> d2) $ \c -> f c || g c

invert :: CardMatcher -> CardMatcher
invert (CardMatcher d f) = CardMatcher ("not " <> d) $ not . f

labelMatch :: String -> CardMatcher -> CardMatcher
labelMatch label (CardMatcher d f) = CardMatcher label f
applyMatcher :: CardMatcher -> Card -> Bool
applyMatcher matcher c =
  case applyMatcherWithDesc matcher c of
    Left _ -> False
    Right _ -> True

loseAttribute attr cn = do
  c <- requireCard cn mempty

  modifyCardDeprecated cn id (removeAttribute attr)

removeAttribute :: CardAttribute -> Card -> Card
removeAttribute attr = over cardAttributes (S.delete attr)

gainAttribute attr cn = do
  c <- requireCard cn mempty

  modifyCardDeprecated cn id (setAttribute attr)

setAttribute :: CardAttribute -> Card -> Card
setAttribute attr = over cardAttributes (S.insert attr)

forCards :: CardMatcher -> (CardName -> GameMonad ()) -> GameMonad ()
forCards matcher f = do
  cs <- allCards

  let matchingCs = filter (applyMatcher matcher) cs

  forM_ (map (view cardName) matchingCs) f

gameFinished :: GameMonad Bool
gameFinished = do
  state <- use phase

  return $ case state of
             Won _ -> True
             _     -> False

getTimestamp :: GameMonad Timestamp
getTimestamp = return 0 -- TODO

askCards :: CardMatcher -> EffectMonad [Card]
askCards matcher = filter (applyMatcher matcher) . map snd . M.toList . view resolvedCards . fst <$> ask
