{-# LANGUAGE TemplateHaskell #-}
module Dovin.Effects
  ( effectPTSet
  , effectPTSetF
  , effectPTAdjustment
  , effectPTAdjustmentF
  , effectNoAbilities
  , effectAddType

  , resolveEffectsV3

  , enabledInPlay

  , viewSelf
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
import Control.Monad.State (get)
import Data.Maybe (mapMaybe)
import Data.List (sortOn)

type Pile = [PileEntry]
data PileEntry = PileEntry
  { _peSource :: Card
  , _peTimestamp :: Timestamp
  , _peEffect :: [LayeredEffectPart]
  , _peAppliesTo :: [CardName]
  }
makeLenses ''PileEntry

-- EFFECT BUILDERS
--
-- These create 'LayeredEffectPart', to be used in effect definitions.
effectPTSet :: (Int, Int) -> LayeredEffectPart
effectPTSet = effectPTSetF . const . pure

effectPTSetF :: (Card -> EffectMonad (Int, Int)) -> LayeredEffectPart
effectPTSetF f = LayeredEffectPart Layer7B $ \c -> do
                   pt <- f c
                   return $ set cardStrength (mkStrength pt) c

effectPTAdjustment :: (Int, Int) -> LayeredEffectPart
effectPTAdjustment = effectPTAdjustmentF . const . pure

effectPTAdjustmentF :: (Card -> EffectMonad (Int, Int)) -> LayeredEffectPart
effectPTAdjustmentF f = LayeredEffectPart Layer7C $ \c -> do
                   pt <- f c
                   return $ over cardStrength (mkStrength pt <>) c

effectNoAbilities = LayeredEffectPart Layer6 (pure . set cardPassiveEffects mempty)

effectAddType attr = LayeredEffectPart Layer4 (pure . over cardAttributes (S.insert attr))

-- EFFECT DEFINITION HELPERS
enabledInPlay :: EffectMonad Bool
enabledInPlay = applyMatcher matchInPlay <$> askSelf

askSelf :: EffectMonad Card
askSelf = snd <$> ask

viewSelf x = view x <$> askSelf

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
