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

type Named n a = (n, a)

type OutName = (FilePath, PackageKey, ModuleName, Name)

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
graphMods :: HsFile -> Ghc [[ModSummary]]
graphMods f = do target <- guessTarget (unHs f) Nothing
                 addTarget target
                 load LoadAllTargets
                 graph <- depanal [] True
                 let sorted = topSortModuleGraph True graph Nothing
                 return (map flattenSCC sorted)

-- | Get all top-level bindings from a Haskell file
bindingsFrom :: HsFile -> Ghc [(PackageKey, ModuleName, [HsBindLR Name Name])]
bindingsFrom f = do sumss <- graphMods f
                    let sums = concat sumss
                    mapM renameMod sums

annotate :: (a, b, [c]) -> [(a, b, c)]
annotate (pid, mn, [])     = []
annotate (pid, mn, (x:xs)) = (pid, mn, x) : annotate (pid, mn, xs)

bindingsFrom' :: HsFile -> Ghc [(PackageKey, ModuleName, HsBindLR Name Name)]
bindingsFrom' f = do bs <- bindingsFrom f
                     return (concatMap annotate bs)

-- | Extract the 'Name' from a binding
namedBinding :: FilePath -> (PackageKey, ModuleName, HsBindLR Name Name) -> Maybe (Named OutName (HsBindLR Name Name))
namedBinding f (pid, mn, e@(FunBind _ _ _ _ _ _)) = Just ((f, pid, mn, unLoc (fun_id e)), e)
namedBinding f (pid, mn, e@(VarBind _ _ _))       = Just ((f, pid, mn, var_id e),         e)
namedBinding f _                                  = Nothing

-- | Gather all bindings from Haskell files and extract their names
namedBindingsFrom :: HsFile -> Ghc [Named OutName (HsBindLR Name Name)]
namedBindingsFrom f = do bindings <- bindingsFrom' f
                         return (mapMaybe (namedBinding (unHs f)) bindings)
