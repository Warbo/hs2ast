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

-- Monad management for IO and Ghc

inDefaultEnv :: GhcMonad m => (HscEnv -> m a) -> m a
inDefaultEnv f = do dflags <- getSessionDynFlags
                    let dflags' = dflags { log_action = dieErr }
                    setSessionDynFlags dflags'
                    liftIO (newHscEnv dflags') >>= f

runGhcM :: (HscEnv -> Ghc a) -> IO a
runGhcM f = runGhc (Just libdir) (inDefaultEnv f)

runIO :: IO a -> IO a
runIO = runGhcM . const . liftIO

-- | Abort GHC when errors are encountered (eg. syntax errors)
dieErr :: LogAction
dieErr dflags severity srcSpan style msg =
  let str    = show (runSDoc locMsg cntx)
      locMsg = mkLocMessage severity srcSpan msg
      cntx   = initSDocContext dflags style
  in  error str

-- | Get all top-level bindings from a list of Haskell files
bindingsFrom :: [HsFile] -> IO [[[HsBindLR Name Name]]]
bindingsFrom = parseFiles

-- | Get the top-level bindings from a list of Haskell files, grouped by module
parseFiles :: [HsFile] -> IO [[[HsBindLR Name Name]]]
parseFiles fs = runGhcM $ (`renameFiles` fs)

-- | Get the top-level bindings from a parsed Haskell module
renameAST ::    HscEnv
             -> ModSummary
             -> HsParsedModule
             -> IO [HsBindLR Name Name]
renameAST env ms pm = do (_, renamed) <- hscTypecheckRename env ms pm
                         case renamed of
                              Nothing            -> error "Did not rename"
                              Just (x, _, _, _)  -> return (extractDefs x)

-- | Get the top-level bindings from a loaded Haskell module
renameMod ::    HscEnv
             -> ModSummary
             -> IO [HsBindLR Name Name]
renameMod env ms = do pm <- hscParse env ms
                      renameAST env ms pm

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
graphMods :: GhcMonad m => HscEnv -> [HsFile] -> m [[ModSummary]]
graphMods env fs = do setSessionDynFlags (hsc_dflags env)
                      mapM ((`guessTarget` Nothing) . unHs) fs >>= setTargets
                      load LoadAllTargets
                      graph <- depanal [] True
                      let sorted = topSortModuleGraph True graph Nothing
                      return (map flattenSCC sorted)

-- | Parse and rename a list of Haskell files in a GHC monad
renameFiles :: GhcMonad m => HscEnv
                          -> [HsFile]
                          -> m [[[HsBindLR Name Name]]]
renameFiles env fs = do graph <- graphMods env fs
                        mapM (mapM (liftIO . renameMod env)) graph
