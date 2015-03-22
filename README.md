# ML4HS: Machine Learning for Haskell #

This is a Haskell port of [ML4PG](https://gitorious.org/ml4pg) (Machine Learning for ProofGeneral). ML4HS will parse Haskell source code, extract information about terms (datatypes, functions, typeclasses, etc.) and perform *clustering*, to group together statistically-similar terms.

These clusters might be used to remove redundancy, for example if you have implemented similar types and functions to some existing library, or for finding abstraction/generalisation opportunities.
