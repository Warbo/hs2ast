module ML4HS.AST where

import Control.Monad.Trans.Class
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax
import Language.Haskell.Names
import qualified Distribution.HaskellSuite.Modules      as M
import qualified Language.Haskell.Exts.Annotated.Syntax as S

parseFile :: String -> IO Module
parseFile f = let mode  = defaultParseMode {parseFilename=f}
                  parse = parseModuleWithMode mode
              in  fmap (fromParseResult . parse) (readFile f)

parseFiles :: [String] -> IO [Module]
parseFiles = mapM parseFile

dump (Module _ _ _ _ _ _ ds) = show ds

--getAst :: FilePath -> M.ModuleInfo IO --S.Module SrcSpan
getAst f = case parse f of
                ParseOk m       -> m
                ParseFailed _ e -> error e

annotateAst :: S.Module SrcSpanInfo -> M.ModuleT [Symbol] IO (S.Module (Scoped SrcSpanInfo))
annotateAst ast = lift $ annotateModule Haskell2010 [] ast
