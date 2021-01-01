{-# LANGUAGE ScopedTypeVariables #-}

module Dovin.Helpers where

import Dovin.Types
import Dovin.Attributes
import Dovin.Prelude
import Dovin.Effects (resolveEffectsV3)
import Dovin.Matchers

import Data.List (sort, sortOn)
import qualified Data.HashMap.Strict as M
import qualified Data.Set as S
import Control.Lens (_1, ASetter, _Just)
import Control.Monad.State (get)
import Control.Monad.Reader (ask, runReader)

import Text.Parsec

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

      return card'

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
