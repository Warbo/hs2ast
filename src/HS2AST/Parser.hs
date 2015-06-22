module HS2AST.Parser where

import Bag
import Data.Maybe
import Digraph
import GHC
import GhcMonad
import Name
import HscMain
import HscTypes
import HS2AST.Types

-- | Get the top-level bindings from a parsed Haskell module
renameAST :: ModSummary -> HsParsedModule -> Ghc [HsBindLR Name Name]
renameAST ms pm = do env <- getSession
                     (_, renamed) <- liftIO $ hscTypecheckRename env ms pm
                     case renamed of
                          Nothing            -> error "Did not rename"
                          Just (x, _, _, _)  -> return (extractDefs x)

-- | Get the top-level bindings from a loaded Haskell module
renameMod :: ModSummary -> Ghc (PackageKey, ModuleName, [HsBindLR Name Name])
renameMod ms = do env <- getSession
                  ast <- liftIO (hscParse env ms)
                  rAst <- renameAST ms ast
                  let mod = ms_mod ms
                  return (modulePackageKey mod, moduleName mod, rAst)

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
bindingsFrom :: [HsFile] -> Ghc [(PackageKey, ModuleName, [HsBindLR Name Name])]
bindingsFrom fs = do sumss <- graphMods fs
                     let sums = concat sumss
                     mapM renameMod sums

annotate :: (a, b, [c]) -> [(a, b, c)]
annotate (pid, mn, [])     = []
annotate (pid, mn, (x:xs)) = (pid, mn, x) : annotate (pid, mn, xs)

bindingsFrom' :: [HsFile] -> Ghc [(PackageKey, ModuleName, HsBindLR Name Name)]
bindingsFrom' fs = do bs <- bindingsFrom fs
                      return (concatMap annotate bs)

-- | Extract the 'Name' from a binding
namedBinding :: (a, b, HsBindLR Name Name) -> Maybe (a, b, Name, HsBindLR Name Name)
namedBinding (pid, mn, e@ (FunBind _ _ _ _ _ _)) = Just (pid, mn, unLoc (fun_id e), e)
namedBinding (pid, mn, e@ (VarBind _ _ _))       = Just (pid, mn, var_id e, e)
namedBinding _                                   = Nothing

-- | Gather all bindings from Haskell files and extract their names
namedBindingsFrom :: [HsFile] -> Ghc [(PackageKey, ModuleName, Name, HsBindLR Name Name)]
namedBindingsFrom fs = do bindings <- bindingsFrom' fs
                          return (mapMaybe namedBinding bindings)
