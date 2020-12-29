{-# LANGUAGE ScopedTypeVariables #-}

module Dovin.Helpers where

import Debug.Trace
import Dovin.Types
import Dovin.Attributes
import Dovin.Prelude

import Data.List (sort, sortOn)
import qualified Data.HashMap.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Data.Char (isDigit)
import Control.Lens (_1, _2, ASetter, both, _Just)
import Control.Monad.State (get)
import Control.Monad.Reader (ask, runReader)

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

-- Unlike the previous effects system, V3 attempts to better mirror the
-- layering rules while also providing a more flexible API to create more types
-- of effects. Some notable constraints this requires solving for include:
--
-- * The set of cards an effect applies to needs to be fixed in the first layer
--   in which the effect would apply.
-- * Layers can create or remove effects in higher layers (e.g. removing all
--   abilities in layer 6). This means it is not possible to know all effects
--   that will be applied at the start of the algorithm, effects need to be
--   collected layer by layer.
--
-- This algorithm uses the concept of a "pile" of unapplied effects, that is
-- both added to and reduced at each layer.
--
-- 1. For every layer, all cards are checked for effects that would start
--    applying on that layer, and all parts of that effect are added to the
--    pile - alongside the set of cards to apply to. For example, "all
--    creatures are 0/1 and have no abilities" applies on both layers 6 and 7B
--    and it will be added to the pile when evaluating layer 6.
-- 2. All sub-effects that apply to the current layer are removed from the pile
--    and evaluated in timestamp order. (Note: dependencies are not implemented
--    yet.)
-- 3. After the final layer, the pile should be empty.
resolveEffectsV3 :: GameMonad ()
resolveEffectsV3 = do
  board <- get
  cs <- resolveEffectsV3' board
  assign resolvedCards cs

  where
    resolveEffectsV3' :: Board -> GameMonad (M.HashMap CardName Card)
    resolveEffectsV3' board = do
      let (newBoard, pile) = foldl resolveLayer (board, mempty) allLayers

      unless (null pile) $ throwError "assertion failed: pile should be empty"

      return $ view resolvedCards newBoard

resolveLayer :: (Board, Pile) -> Layer -> (Board, Pile)
resolveLayer (board, pile) layer =
  let 
    cs            = view resolvedCards board
    newEffects    = concatMap (extractEffects layer) cs :: Pile
    newPile       = pile <> newEffects :: Pile
    (pile', peel) = peelLayer layer newPile
    newBoard      = foldl applyEffects board . sortOn (view peTimestamp) $ peel
  in
  (newBoard, pile')

  where
    -- Take a PileEntry and apply it to the board state. It is assumed that it
    -- has already been filtered to a single layer.
    applyEffects :: Board -> PileEntry -> Board
    applyEffects board pe =
      let
        cs = mapMaybe (\cn -> M.lookup cn (view resolvedCards board))
             . view peAppliesTo
             $ pe :: [Card]
        newCards = map
                     (applyEffectParts (view peSource pe) (view peEffect pe))
                     cs
      in

      over
        resolvedCards
        (M.union . M.fromList . map (\c -> (view cardName c, c)) $ newCards)
        board

    applyEffectParts source es target =
      foldl
        (\t (LayeredEffectPart _ effect) ->
          runReader (effect t) (board, source))
        target
        es

    -- Find all effects on a card that begin applying at the given layer.
    extractEffects :: Layer -> Card -> Pile
    extractEffects layer c =
      let
        passiveEffects =
          map toPileEntry
          . filter isEnabled
          . view cardPassiveEffects
          $ c
        abilityEffects =
          map (\(AbilityEffect t _ es) ->
            PileEntry {
              _peSource = c,
              _peTimestamp = t,
              _peEffect = es,
              _peAppliesTo = [view cardName c]
            })
          . view cardAbilityEffects
          $ c
      in

      filter
        ((==) layer . minimum . map extractLayer . view peEffect)
        (passiveEffects <> abilityEffects)

      where
        extractLayer (LayeredEffectPart l _) = l

        isEnabled :: LayeredEffectDefinition -> Bool
        isEnabled ld = runReader (view leEnabled ld) (board, c)

        toPileEntry :: LayeredEffectDefinition -> PileEntry
        toPileEntry ld =
          let
            matcher = runReader (view leAppliesTo ld) (board, c)
            cs' =
              filter
                (applyMatcher matcher)
                (M.elems . view resolvedCards $ board)
          in

          PileEntry {
            _peSource = c,
            _peTimestamp = view cardTimestamp c,
            _peEffect = view leEffect ld,
            _peAppliesTo = map (view cardName) cs'
          }

-- Return two piles, the second including every effect part that applies at
-- this layer, the first with all the remaining. Removes any entries that no
-- longer have any effect parts remaining to apply.
peelLayer :: Layer -> Pile -> (Pile, Pile)
peelLayer layer pile =
  (f not pile, f id pile)
  where
    f g =
      filter (not . null . view peEffect)
      . map (over peEffect (filter $ g . isLayer layer))

    isLayer :: Layer -> LayeredEffectPart -> Bool
    isLayer l1 (LayeredEffectPart l2 _) = l1 == l2

resolveEffects :: GameMonad ()
resolveEffects = do
  board <- get
  cs <- resolveEffects' board
  assign resolvedCards cs
  modifying currentTime (+ 1)
  resolveEffectsV3

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

      -- TODO: Should be handled in Layer7C in V3 effects system
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

matchCard :: Card -> CardMatcher
matchCard = matchName . view cardName

matchToughness :: Int -> CardMatcher
matchToughness n = labelMatch ("toughness = " <> show n) $ CardMatcher ""
  ((== n) . view cardToughness) <> matchAttribute creature

matchTarget :: Target -> CardMatcher
matchTarget t = labelMatch ("target = " <> show t) $ CardMatcher ""
  ((==) t . TargetCard . view cardName)

enabledInPlay :: EffectMonad Bool
enabledInPlay = applyMatcher matchInPlay <$> askSelf

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
getTimestamp = use currentTime

askCards :: CardMatcher -> EffectMonad [Card]
askCards matcher = filter (applyMatcher matcher) . map snd . M.toList . view resolvedCards . fst <$> ask
