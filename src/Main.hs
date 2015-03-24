module Main where

import Data.Set as Set
import Language.Haskell.Exts.Annotated hiding (ann)
import Distribution.HaskellSuite.Modules
import Distribution.HaskellSuite.Packages
import Language.Haskell.Names
import Language.Haskell.Names.Interfaces

--parseFile :: String -> IO (Module SrcLoc)
parseF f = do let mode = defaultParseMode {parseFilename=f}
                  parse = parseModuleWithMode mode
              src <- readFile f
              return (fromParseResult (parse src))

--ann :: Module SrcSpan -> ModuleT [Symbol] IO (Module (Scoped SrcSpan))
ann = annotateModule Haskell2010 []

compInt = computeInterfaces Haskell2010 []

compIntErr ms = do errs <- compInt ms
                   if Set.null errs
                      then return ms
                      else error (show (elems errs))

parseFiles = mapM parseF

uniquify ms = compIntErr ms >>= mapM ann

--parseUniqueStrs ss = evalNamesModuleT undefined (uniquify ss)

{-
parseUnique fs = do parsed <- parseFiles fs
                    evalNamesModuleT undefined (uniquify parsed)
-}
--main = evalNamesModuleT (parseUnique ["tests/data/arith.hs"]) (getInstalledPackages)
