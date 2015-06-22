module Mod2AST where

import HS2AST
import System.IO.Temp
import Test.Arbitrary.Cabal
import Test.Arbitrary.Haskell

mod2Ast = print "hello world"

           -- Get our input files from stdin
mod2Ast2 = do input <- getContents

              -- Build ASTs
              result <- filesToAsts (lines input)

              -- Print results to stdout
              putStr (unlines result)

--mkCabal :: IO a
mkCabal = withSystemTempDirectory "hs2ast" useCabal

--useCabal :: FilePath -> IO ()
useCabal dir = do makeProject dir genThisProject

genThisProject = genProject []

genProject pkgs = P { name = "hs2astTemp"
                    , version = [1]
                    , headers  = S () [
                        ("cabal-version", ">= 1.8")
                      , ("build-type", "Simple")
                      , ("category", "Language")
                      , ("maintainer", "nobody@example.com")
                      , ("description", "Temporary pkg")
                      , ("synopsis", "Temporary pkg")
                      , ("license", "GPL")
                      , ("license-file", "LICENSE")
                      ]
                    , sections = [
                        S "executable Mods2Asts" [
                            ("main-is", "Main.hs")
                          ]
                      ]
                    , files = [(([],"Main.hs"), H "")]
                    }
