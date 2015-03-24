module Main where

import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax

parseFile :: String -> IO Module
parseFile f = let mode  = defaultParseMode {parseFilename=f}
                  parse = parseModuleWithMode mode
              in  fmap (fromParseResult . parse) (readFile f)

parseFiles = mapM parseFile

dump (Module _ _ _ _ _ _ ds) = show ds

main = parseFiles ["tests/data/arith.hs"]
