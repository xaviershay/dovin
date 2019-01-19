# Dovin

A proof assistant for [Possibility
Storm](http://www.possibilitystorm.com/)-style Magic: The Gathering puzzles.

> He has an innate talent, heightened by magic, that allows him to clearly see
> the flaws in any system or machine. After mere moments of scrutiny, Dovin can
> provide a complete analysis, noting a particular machine's weaknesses,
> highlighting its shortcomings, and predicting with startling accuracy exactly
> how and when it will fail. --- [Dovin Baan, Planeswalker](https://magic.wizards.com/en/story/planeswalkers/dovin-baan)

It provides a haskell DSL for expressing Magic actions, with tracking of life
and other counters and validation of pre- and post- conditions. For example, if
you try to tap a permanent that is already tapped, it will error. Or if you try
to target a creature with hexproof. It can also track state-based effects, such
as "all creatures get +1/+1" or "other creatures gain hexproof".

It does not try to provide a full ruleset implementation, instead it's more
akin to letting you lay out cards and counters in front of you and manipulate
them as you would in a real game of paper magic.

I've only added actions "as needed" to solve problems, so the built-in
functions are rather incomplete right now. It is straightforward to add more
though. See `src/Dovin/V2.hs` in conjuction with `src/Dovin/Actions.hs` for
supported and tested actions, and `src/Dovin/Dump.hs` for untested experimental
ones.

## Example

``` haskell
module Solutions.Example where

import Dovin

main = run formatter solution

solution :: GameMonad ()
solution = do
  step "Initial state" $ do
    setLife Opponent 3

    withLocation Hand $ addInstant "Plummet"
    withLocation Play $ do
      addLands 2 "Forest"

    as Opponent $ do
      withLocation Play $ do
        withAttributes [flying, token] $ addCreature (4, 4) "Angel"
        withAttributes [flying]
          $ withEffect
              matchInPlay
              (matchOtherCreatures <> (const $ matchAttribute creature))
              (pure . setAttribute hexproof)
          $ addCreature (3, 4) "Shalai, Voice of Plenty"

  step "Plummet to destroy Shalai" $ do
    tapForMana "G" (numbered 1 "Forest")
    tapForMana "G" (numbered 2 "Forest")
    cast "1G" "Plummet"
    resolve "Plummet"
    with "Shalai, Voice of Plenty" $ \enemy -> do
      target enemy
      validate (matchAttribute flying) enemy
      destroy enemy

formatter :: Step -> Formatter
formatter step = case view stepNumber step of
  1 -> manaFormatter
    <> cardFormatter "opponent creatures" (matchLocation (Opponent, Play))
  _ -> boardFormatter

manaFormatter = attributeFormatter $ do
  attribute "availble mana" $
    countCards (matchAttribute land <> missingAttribute tapped)
```

`run` uses the supplied formatter to print out the board state at each step:

    1. Initial state
          (Active,Hand):
            Plummet (instant)
          (Active,Play):
            Forest 1 (land)
            Forest 2 (land)
          (Opponent,Play):
            Angel (creature,flying,hexproof,token) (4/4, 0)
            Shalai, Voice of Plenty (creature,flying) (3/4, 0)
    2. Plummet to destroy Shalai
          availble mana: 0
          opponent creatures:
            Angel (creature,flying,token) (4/4, 0)

## Solutions Index

See `src/Solutions` for more extensive usage (spoiler alert: these are
solutions for published Possibility Storm puzzles!)

* `Dominaria5` uses a planeswalker.
* `RivalsOfIxalan7` uses `withEffect` to model `exert`
  effects, and shows how to verify multiple blocking scenarios.
* `Core19_9` has a fancy formatter to correctly track how much
  mana is available when working with `Powerstone Shard`, as well as spell
  tracking for `Aetherflux Reservoir`.
* `GuildsOfRavnicaPre2` uses `forCards` to model undergrowth
  for a `Rhizome Lurcher`.
* `GuildsOfRavnica1` uses `mentor`.
* `GuildsOfRavnica3` uses a sacrifice wrapper to repeatedly
  create treasure tokens.
* `GuildsOfRavnica8` shows using counters to correctly track
  `Muldrotha, the Gravetide` usage.
* `GuildsOfRavnica9` handles `storm`.
* `ExplorersOfIxalanContest` handles some pretty weird damage
  interactions.
* `UltimateMasters` shows how to track opponent actions.
* `ChannelFireball` automatically calculates High Tide mana.

## Development

    bin/dev  # Launch an interactive REPL
    bin/test # Runs all tests and lints
    bin/run  # Runs all solutions

`src/Dovin/Dump.hs` is currently a dumping ground for prototype code. Actions
are in the process of being moved to `Dovin.Actions`. To be moved they must:

  * Be unit tested.
  * Be documented.
  * The primary target card of an action should be the final parameter.
