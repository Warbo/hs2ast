module HS2AST.Parser where

import CoreSyn
import Bag
import Control.Monad
import Control.Applicative
import Data.Maybe
import Digraph
import DynFlags
import ErrUtils
import Exception
import GHC
import GHC.Paths
import GhcMonad
import HsDecls
import HsBinds
import Name
import HscMain
import HscTypes
import Outputable
import HS2AST.Types
import System.IO
import System.Directory
import Control.Monad.IO.Class

-- | Get the top-level bindings from a parsed Haskell module
renameAST :: ModSummary -> HsParsedModule -> Ghc [HsBindLR Name Name]
renameAST ms pm = do env <- getSession
                     (_, renamed) <- liftIO $ hscTypecheckRename env ms pm
                     case renamed of
                          Nothing            -> error "Did not rename"
                          Just (x, _, _, _)  -> return (extractDefs x)

-- | Get the top-level bindings from a loaded Haskell module
renameMod :: ModSummary -> Ghc [HsBindLR Name Name]
renameMod ms = do env <- getSession
                  ast <- liftIO (hscParse env ms)
                  renameAST ms ast

-- | Extract top-level definitions from a module AST
extractDefs :: HsGroup id -> [HsBindLR id id]
extractDefs g = let vals  = hs_valds g
                    bag   = case vals of
                                 ValBindsIn  _ _   -> error "Not renamed?"
                                 ValBindsOut bs ss -> map snd bs
                    binds = concatMap bagToList bag
                    unloc = map (\(L _ e) -> e) binds
                in  unloc

-- | Get a topologically sorted graph of Haskell modules from the given files.
graphMods :: [HsFile] -> Ghc [[ModSummary]]
graphMods fs = do targets <- mapM ((`guessTarget` Nothing) . unHs) fs
                  mapM_ addTarget targets
                  load LoadAllTargets
                  graph <- depanal [] True
                  let sorted = topSortModuleGraph True graph Nothing
                  return (map flattenSCC sorted)

-- | Get all top-level bindings from a list of Haskell files
bindingsFrom :: [HsFile] -> Ghc [[[HsBindLR Name Name]]]
bindingsFrom fs = graphMods fs >>= mapM (mapM renameMod)

bindingsFrom' :: [HsFile] -> Ghc [HsBindLR Name Name]
bindingsFrom' fs = do bs <- bindingsFrom fs
                      return (concat (concat bs))

showSdoc x = do flags <- getSessionDynFlags
                return (show (runSDoc (ppr x) (initSDocContext flags defaultDumpStyle)))

-- | Extract the 'Name' from a binding
namedBinding :: HsBindLR Name Name -> Maybe (Name, HsBindLR Name Name)
namedBinding e@(FunBind _ _ _ _ _ _) = Just (unLoc (fun_id e), e)
namedBinding e@(VarBind _ _ _)       = Just (var_id e, e)
namedBinding _                       = Nothing

-- | Render a binding's name to a String
strBinding :: (GhcMonad m, Outputable a) => (a, t) -> m (String, t)
strBinding (n, e) = do n' <- showSdoc n
                       return (n', e)

-- | Gather all bindings from Haskell files and extract their names
namedBindingsFrom :: [HsFile] -> Ghc [(Name, HsBindLR Name Name)]
namedBindingsFrom fs = do bindings <- bindingsFrom' fs
                          return (mapMaybe namedBinding bindings)

withTempHaskell :: MonadIO m => Haskell -> (HsFile -> m a) -> m a
withTempHaskell (H s) f = do
  p <- liftIO $ bracket (openTempFile "/tmp" "hs2ast_temp.hs")
                        (\(p, h) -> hClose h)
                        (\(p, h) -> hPutStr h s >> return p)
  result <- f (fromJust (mkHs p))
  liftIO $ removeFile p
  return result

-- | Parse a Haskell string (using a temporary file!)
parseHaskell :: Haskell -> Ghc [[[HsBindLR Name Name]]]
parseHaskell h = fmap dummyTypes $ withTempHaskell h (\p -> bindingsFrom [p])

coreVals :: HsFile -> Ghc CoreProgram
coreVals f = cm_binds <$> compileToCoreSimplified (unHs f)

coreTypes :: HsFile -> Ghc TypeEnv
coreTypes f = cm_types <$> compileToCoreSimplified (unHs f)
