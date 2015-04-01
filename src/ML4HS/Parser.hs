module ML4HS.Parser where

import Bag
import Control.Monad
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
import ML4HS.Types




-- | Get all top-level bindings from a list of Haskell files
bindingsFrom :: [HsFile] -> InSession [[[HsBindLR Name Name]]]
bindingsFrom = parseFiles

-- | Get the top-level bindings from a list of Haskell files, grouped by module
parseFiles :: [HsFile] -> InSession [[[HsBindLR Name Name]]]
parseFiles fs = renameFiles fs

-- | Get the top-level bindings from a parsed Haskell module
renameAST :: ModSummary -> HsParsedModule -> InSession [HsBindLR Name Name]
renameAST ms pm = inSession (\env -> do (_, renamed) <- liftIO $ hscTypecheckRename env ms pm
                                        case renamed of
                                             Nothing            -> error "Did not rename"
                                             Just (x, _, _, _)  -> return (extractDefs x))

-- | Get the top-level bindings from a loaded Haskell module
renameMod :: ModSummary -> InSession [HsBindLR Name Name]
renameMod ms = inSession (liftIO . (`hscParse` ms)) >>= renameAST ms

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
graphMods :: [HsFile] -> InSession [[ModSummary]]
graphMods fs = inSession (\env -> do mapM ((`guessTarget` Nothing) . unHs) fs >>= setTargets
                                     load LoadAllTargets
                                     graph <- depanal [] True
                                     let sorted = topSortModuleGraph True graph Nothing
                                     return (map flattenSCC sorted))

-- | Parse and rename a list of Haskell files in a GHC monad
renameFiles :: [HsFile] -> InSession [[[HsBindLR Name Name]]]
renameFiles fs = graphMods fs >>= mapM (mapM renameMod)
