# ML4HS: Machine Learning for Haskell #

This is a Haskell port of [ML4PG](https://gitorious.org/ml4pg) (Machine Learning for ProofGeneral). ML4HS will parse Haskell source code, extract information about terms (datatypes, functions, typeclasses, etc.) and perform *clustering*, to group together statistically-similar terms.

These clusters might be used to remove redundancy, for example if you have implemented similar types and functions to some existing library, or for finding abstraction/generalisation opportunities.

# TODO

## Tools

Look at github.com/tip-org/tip-tools -- Haskell-parsing frontend to GHC
Look at hbmc                         -- Haskell Bounded Model Checker; example of using tip-tools
Look at Haste                        -- Works with packages, modules, etc. (for later)
Maybe look at LiquidHaskell          -- Similar approach to tip-tools (but probably not standalone)

## Approach

 - Write Haskell definitions to s-expr with backlinks
 - Use IDs in s-exprs, so also write an ID <-> Haskell name table (MD5s for IDs?)
 - Write simple s-expr parser (2 mins work with Parsec)
 - Generalise TreeFeatures to work with arbitrary rose trees, not just XML
 - Write out table of features <-> s-expr IDs
 - Throw features into Weka
 - Run
