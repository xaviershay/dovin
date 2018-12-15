# Dovin

A proof assistant for [Possibility
Storm](http://www.possibilitystorm.com/)-style Magic: The Gathering puzzles.

> He has an innate talent, heightened by magic, that allows him to clearly see
> the flaws in any system or machine. After mere moments of scrutiny, Dovin can
> provide a complete analysis, noting a particular machine's weaknesses,
> highlighting its shortcomings, and predicting with startling accuracy exactly
> how and when it will fail. --- [Dovin Baan, Planeswalker](https://magic.wizards.com/en/story/planeswalkers/dovin-baan)

It provides a haskell DSL for expressing Magic actions, with tracking of life and other
counters and validation of pre- and post- conditions. For example, if you try
to tap a permanent that is already tapped, it will error. Or if you try to
target a creature with hexproof.

It does not try to provide a full ruleset implementation, instead it's more
akin to letting you lay out cards and counters in front of you and manipulate
them as you would in a real game of paper magic.

I've only added actions "as needed" to solve problems, so the built-in
functions are rather incomplete right now. It is straightforward to add more
though. See `src/Dovin.hs` and `src/Dovin/Actions.hs` for available actions.

## Example

``` haskell
import Control.Monad (forM_)

import Dovin

main = runVerbose solution

solution :: GameMonad ()
solution = do
  step "Initial state" $ do
    setLife Opponent 3

    addCard "Plummet" (Active, Hand) ["instant"]
    addCards 2 "Forest" (Active, Play) ["land"]
    addToken "Angel" (4, 4) (Opponent, Play) ["angel", "flying"]

  step "Plummet to destroy angel" $ do
    forM_ [1..2] $ \n -> tapForMana "G" (numbered n "Forest")
    cast "1G" "Plummet"
    resolve "Plummet"
    with "Angel" $ \enemy -> do
      target enemy
      validate enemy $ matchAttribute "flying"
      destroy enemy
```

`runVerbose` prints out the board state at each step:

    1. Initial state

    Opponent Life: 3

    (Active,Hand)
      Plummet (instant)
    (Active,Play)
      Forest 1 (land)
      Forest 2 (land)
    (Opponent,Play)
      Angel (angel,creature,flying,token) (4/4, 0)


    2. Plummet to destroy angel

    Opponent Life: 3

    (Active,Graveyard)
      Plummet (instant)
    (Active,Play)
      Forest 1 (land,tapped)
      Forest 2 (land,tapped)

See `src/Solutions` for more extensive usage (spoiler alert: these are
solutions for published Possibility Storm puzzles!)

## Development

    stack build
    stack exec dovin # Runs all solutions

`src/Dovin.hs` is currently a dumping ground for prototype code. Actions are in
the process of being moved to `Dovin.Actions`. To be moved they must:

  * Be unit tested.
  * Be documented.
