module Mod2AST where

import Data.List
import HS2AST
import System.IO.Temp
import Test.Arbitrary.Cabal
import Test.Arbitrary.Haskell

testMain = print "hello world"

           -- Get our input modules from stdin
mod2Ast = do input <- getContents

             -- Build ASTs
             result <- modsToAsts (lines input)

             -- Print results to stdout
             putStr (unlines result)

modsToAsts = return . map reverse

--mkCabal :: IO a
mkCabal pkgs main = withSystemTempDirectory "hs2ast" (useCabal pkgs main)

--useCabal :: FilePath -> IO ()
useCabal pkgs main dir = do makeProject dir (genProject pkgs main)

genProject pkgs main = P {
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
      (([], "Main.hs"), H (unlines [
        "module Main where",
        "import Mod2AST",
        "main = " ++ main
      ]))
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
