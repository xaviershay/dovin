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

## Example

    main = do
      runVerbose $ do
        setLife 3

        addCard "Plummet" (Active, Hand) ["instant"]
        addCards 2 "Forest" (Active, Play) ["land"]
        addCard "Angel" (4, 4) (Opponent, Play) ["angel", "flying", "token"]

        cast "Plummet"
        resolve "Plummet"
        with "Angel" $ \enemy -> do
          target enemy
          validate enemy $ matchAttribute "flying"
          destroy enemy

See `src/Solutions` for more extensive usage (spoiler alert: these are
solutions for published Possibility Storm puzzles!)

## Development

    stack build
    stack exec dovin -- Runs all solutions
