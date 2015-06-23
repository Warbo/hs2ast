module Mod2AST where

import Data.List
import HS2AST
import System.IO.Temp
import Test.Arbitrary.Cabal
import Test.Arbitrary.Haskell

mod2Ast = print "hello world"

           -- Get our input modules from stdin
mod2Ast2 = do input <- getContents

              -- Build ASTs
              result <- modsToAsts (lines input)

              -- Print results to stdout
              putStr (unlines result)

modsToAsts = return . map reverse

--mkCabal :: IO a
mkCabal pkgs = withSystemTempDirectory "hs2ast" (useCabal pkgs)

--useCabal :: FilePath -> IO ()
useCabal pkgs dir = do makeProject dir (genProject pkgs)

genProject pkgs = P {
    name = "hs2astTemp"
  , version = [1]
  , headers  = S () [
      ("cabal-version", ">= 1.8")
    , ("build-type",    "Simple")
    , ("category",      "Language")
    , ("maintainer",    "nobody@example.com")
    , ("description",   "Temporary pkg")
    , ("synopsis",      "Temporary pkg")
    , ("license",       "GPL")
    , ("license-file",  "LICENSE")
    ]
  , sections = [
    S "executable Mods2Asts" [
        ("main-is", "Main.hs")
      , ("build-depends", intercalate "," (pkgs ++ hs2astDeps))
      ]
    ]
  , files = [
      (([], "Main.hs"), H "")
    ]
  }

hs2astDeps = [ "base"
             , "ghc"
             , "ghc-paths"
             , "QuickCheck"
             , "uniplate"
             , "transformers"
             , "syb"
             , "directory"
             , "bytestring"
             , "containers"
             , "ArbitraryHaskell"
             , "temporary"
             ]
