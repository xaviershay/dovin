{-# LANGUAGE TemplateHaskell #-}

{-|
Effects are continuous effects, such as "other creatures get +1/+1". Note that
dependencies (613.8) are not implemented, and must be emulated with correct
timestamps if needed.

They are typically added to a card using 'Dovin.Builder.withEffect' or 'Dovin.Actions.addEffect'.
 -}
module Dovin.Effects
  ( effectPTSet
  , effectPTSetF
  , effectPTAdjust
  , effectPTAdjustF
  , effectNoAbilities
  , effectAddAbility
  , effectAddType
  , effectProtectionF
  , effectControl
  , effectControlF

  , resolveEffects

  , enabledInPlay

  , viewSelf
  , askCards
  , askSelf
  )
  where

import Dovin.Prelude
import Dovin.Types
import Dovin.Matchers (applyMatcher, matchInPlay)

import Control.Lens (makeLenses, over, view, set)
import qualified Data.HashMap.Strict as M
import qualified Data.Set as S
import Control.Monad.Reader (ask, runReader)
import Control.Monad.State (modify', runState, State, get)
import Data.Maybe (mapMaybe, catMaybes)
import Data.List (sortOn, partition)
import Data.Tuple (swap)

type Pile = [PileEntry]
data PileEntry = PileEntry
  { _peSource :: Card
  , _peTimestamp :: Timestamp
  , _peEffect :: [LayeredEffectPart]
  , _peAppliesTo :: Either (EffectMonad CardMatcher) [CardName]
  }
makeLenses ''PileEntry

-- | Constant variant of 'effectPTSetF'.
effectPTSet :: (Int, Int) -> LayeredEffectPart
effectPTSet = effectPTSetF . const . pure

-- | Layer 7B effect to set the power and toughness of a creature.
effectPTSetF :: (Card -> EffectMonad (Int, Int)) -> LayeredEffectPart
effectPTSetF f = LayeredEffectPart Layer7B $ \c -> do
                   pt <- f c
                   return $ set cardStrength (mkStrength pt) c

-- | Constant variant of 'effectPTAdjustF'
effectPTAdjust :: (Int, Int) -> LayeredEffectPart
effectPTAdjust = effectPTAdjustF . const . pure

-- | Layer 7C effect to adjust the power and toughness of a creature.
effectPTAdjustF :: (Card -> EffectMonad (Int, Int)) -> LayeredEffectPart
effectPTAdjustF f = LayeredEffectPart Layer7C $ \c -> do
                   pt <- f c
                   return $ over cardStrength (mkStrength pt <>) c

-- | Layer 6 effect to add an ability to a card. In practice, it adds adds a
-- new 'CardAttribute'.
effectAddAbility attr = LayeredEffectPart Layer6 (pure . over cardAttributes (S.insert attr))

-- | Layer 6 effect to remove all abilities from a card. This doesn't
-- temporary abilities added by 'addEffect'.
effectNoAbilities = LayeredEffectPart Layer6 (pure . set cardPassiveEffects mempty)

-- | Layer 4 effect to add a type to a card. Since card types are modeled
-- explicitly, it instead adds a new 'CardAttribute'.
effectAddType attr = LayeredEffectPart Layer4 (pure . over cardAttributes (S.insert attr))

effectProtectionF :: (Card -> EffectMonad Colors) -> LayeredEffectPart
effectProtectionF f = LayeredEffectPart Layer6 $ \c -> do
                        cs <- f c
                        return $ over cardProtection (cs <>) c

effectControl = effectControlF . const . pure

effectControlF :: (Card -> EffectMonad Player) -> LayeredEffectPart
effectControlF f = LayeredEffectPart Layer2 $ \c -> do
                     p <- f c
                     return $ over cardController (const p) c

-- | Effect enabled definition to apply when a card is in play.
enabledInPlay :: EffectMonad Bool
enabledInPlay = applyMatcher matchInPlay <$> askSelf

-- | The card that is generating the effect being applied.
askSelf :: EffectMonad Card
askSelf = snd <$> ask

-- | Apply a lens to 'askSelf'.
viewSelf x = view x <$> askSelf

-- | Return cards fitting the given matcher.
askCards :: CardMatcher -> EffectMonad [Card]
askCards matcher =
  filter (applyMatcher matcher)
  . M.elems
  . view resolvedCards
  . fst
  <$> ask

-- | Internal algorithm to apply re-calculate the state of the board by applying all effects.
resolveEffects :: GameMonad ()
resolveEffects = do
  -- This just happens to be a convenient place to bump the timestamp. SBE
  -- handling might be a better spot though.
  modifying currentTime (+ 1)

  modify' resetCards
  modify' resolveCounters
  modify' applyEffects

resetCards :: Board -> Board
resetCards board = set resolvedCards (M.map unwrap . view cards $ board) board
  where
    unwrap (BaseCard card) = card

-- Unlike the previous effects system, V3 attempts to better mirror the
-- layering rules while also providing a more flexible API to create more
-- types of effects. Some notable constraints this requires solving for
-- include:
--
-- * The set of cards an effect applies to needs to be fixed in the first
--   layer in which the effect would apply.
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
--    creatures are 0/1 and have no abilities" applies on both layers 6 and
--    7B and it will be added to the pile when evaluating layer 6.
-- 2. All sub-effects that apply to the current layer are removed from the
--    pile and evaluated in timestamp order. (Note: dependencies are not
--    implemented yet.)
-- 3. After the final layer, the pile should be empty.
applyEffects :: Board -> Board
applyEffects board =
  let
    f accum v = applyEffectsAtLayer v . collectNewEffectsAtLayer v $ accum
    (newBoard, pile) = foldl f (board, mempty) allLayers
  in

  if null pile then
    newBoard
  else
    error "assertion failed: pile should be empty"

-- Convert card counters and legacy strength modifiers into V3 effects. Note
-- that since counter timestamps are not implemented, card timestamps are used
-- instead.
resolveCounters :: Board -> Board
resolveCounters board =
  let
    newCards = M.map countersToEffect . view resolvedCards $ board
  in

  set resolvedCards newCards board

  where
    unwrap (BaseCard card) = card

    countersToEffect :: Card -> Card
    countersToEffect card =
      let es =
            map (AbilityEffect (view cardTimestamp card) EndOfTurn . replicate 1)
            . catMaybes
            . map (\f -> f card)
            $ [ mkPTEffect . dup . view cardPlusOneCounters
              , mkPTEffect . dup . view cardMinusOneCounters
              , mkPTEffect . toTuple . view cardStrengthModifier
              ]
      in

      over
        cardAbilityEffects
        (es <>)
        card

      where
        toTuple (CardStrength p t) = (p, t)
        mkPTEffect :: (Int, Int) -> Maybe LayeredEffectPart
        mkPTEffect (p, t) =
          -- This is an optimization to not create a large number of no-op
          -- effects.
          if p == 0 && t == 0 then
            Nothing
          else
            Just $ effectPTAdjust (p, t)

collectNewEffectsAtLayer :: Layer -> (Board, Pile) -> (Board, Pile)
collectNewEffectsAtLayer layer (board, pile) =
  (
    board,
    sortOn (view peTimestamp) . (pile <>) . concatMap (extractCardEffects layer) . view resolvedCards $ board
  )

  where
    -- Find all effects on a card that begin applying at the given layer.
    extractCardEffects :: Layer -> Card -> Pile
    extractCardEffects layer c =
      let
        passiveEffects =
          map ldToPileEntry
          . view cardPassiveEffects
          $ c
        abilityEffects =
            map aeToPileEntry
          . view cardAbilityEffects
          $ c
      in

      filter
        ((==) layer . minimum . map extractLayer . view peEffect)
        (passiveEffects <> abilityEffects)

      where
        aeToPileEntry :: AbilityEffect -> PileEntry
        aeToPileEntry (AbilityEffect t _ es) =
          PileEntry {
            _peSource = c,
            _peTimestamp = t,
            _peEffect = es,
            _peAppliesTo = Right [view cardName c]
          }

        ldToPileEntry :: LayeredEffectDefinition -> PileEntry
        ldToPileEntry ld =
          PileEntry {
            _peSource = c,
            _peTimestamp = view cardTimestamp c,
            _peEffect = view leEffect ld,
            _peAppliesTo = Left (view leAppliesTo ld)
          }

-- Apply all effects in the pile to the board for the given layer.
applyEffectsAtLayer :: Layer -> (Board, Pile) -> (Board, Pile)
applyEffectsAtLayer layer (board, pile) =
  -- Iterate over all pile entries and attempt to apply/reduce them
  swap . runState (catMaybes <$> mapM (applyEntry layer) pile) $ board

  where
    -- Apply (and remove) any effect parts (may be none) in the entry for the
    -- current layer. If this is the first time a part would apply for this
    -- effect, also resolve the CardMatcher to determine the set of cards to
    -- apply to.
    --
    -- The use of State monad here is perhaps a little weird, but was the best
    -- I could come with to structure the code.
    applyEntry :: Layer -> PileEntry -> State Board (Maybe PileEntry)
    applyEntry layer pe = do
      -- Split the remaining entry effects into those that apply in the current
      -- layer, and those that don't.
      let (parts, remainder) = partition (isLayer layer) (view peEffect pe)

      if null parts then
        -- If no parts apply at current layer, there is nothing to be done.
        return (Just pe)
      else do
        board <- get

        let effectEnv = (board, view peSource pe)

        -- If the entire effect hasn't previously decided which set of cards to
        -- apply to, it does so here.
        let cns = either (resolveAppliesTo effectEnv) id (view peAppliesTo pe)

        -- Look up the affected cards in the current board state.
        let cs = mapMaybe (\cn -> M.lookup cn (view resolvedCards board)) cns

        when (length cs /= length cns)
          -- Since SBEs don't apply while we are applying effects, there should
          -- be no opportunity for a card to be removed.
          (error "assertion failed: effected card did not exist on board")

        -- Apply the effect part to the matched cards
        let newCs = map (applyEffectParts effectEnv parts) cs

        -- Update the board state with the newly updated cards.
        modifying
          resolvedCards
          (M.union . M.fromList . indexBy (view cardName) $ newCs)

        return $
          if null remainder then
            Nothing
          else
            Just
            -- The set of cards needs to be cached here for use by future
            -- parts. It should not be recalculated per 613.6.
            . set peAppliesTo (Right cns)
            -- We also remove the applied parts (the current layer) from the
            -- effect.
            . set peEffect remainder
            $ pe

    resolveAppliesTo :: EffectMonadEnv -> EffectMonad CardMatcher -> [CardName]
    resolveAppliesTo (board, source) m =
        map (view cardName)
      . filter (applyMatcher $ runReader m (board, source))
      . M.elems
      . view resolvedCards
      $ board

    applyEffectParts :: EffectMonadEnv -> [LayeredEffectPart] -> Card -> Card
    applyEffectParts env es target =
      foldl
        (\t (LayeredEffectPart _ eff) -> runReader (eff t) env)
        target
        es

indexBy :: (a -> b) -> [a] -> [(b, a)]
indexBy f = map ((,) <$> f <*> id)

extractLayer (LayeredEffectPart l _) = l

isLayer :: Layer -> LayeredEffectPart -> Bool
isLayer l p = l == extractLayer p
