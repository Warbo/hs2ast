module ML4HS.Parser where

import Bag
import Control.Monad
--import Control.Monad.IO.Class
import DynFlags
import ErrUtils
import Exception
import GHC
import GHC.Paths
import GhcMonad
import HsDecls
import HscMain
import HscTypes
import Outputable

inDefaultEnv :: GhcMonad m => (HscEnv -> m a) -> m a
inDefaultEnv f = do dflags <- getSessionDynFlags
                    let dflags' = dflags { log_action = dieErr }
                    setSessionDynFlags dflags'
                    liftIO (newHscEnv dflags') >>= f

runGhcM  = runGhc (Just libdir)

runIO = runGhc (Just libdir) . liftIO

renameAST env ms pm = do (_, renamed) <- hscTypecheckRename env ms pm
                         case renamed of
                              Nothing            -> error "Did not rename"
                              Just (x, _, _, _)  -> return (extractDefs x)

renameMod env ms = do pm <- hscParse env ms
                      renameAST env ms pm

extractDefs g = let vals  = hs_valds g
                    bag   = case vals of
                                 ValBindsIn  _ _   -> error "Not renamed?"
                                 ValBindsOut bs ss -> map snd bs
                    binds = concatMap bagToList bag
                    unloc = map (\(L _ e) -> e) binds
                in  unloc

graphMods env fs = do setSessionDynFlags (hsc_dflags env)
                      mapM (`guessTarget` Nothing) fs >>= setTargets
                      load LoadAllTargets
                      depanal [] True

renameGraph env fs = do mods <- graphMods env fs
                        return $ mapM (renameMod env) mods

graphFiles env fs = renameGraph env fs

dieErr :: LogAction
dieErr dflags severity srcSpan style msg =
  let str    = show (runSDoc locMsg cntx)
      locMsg = mkLocMessage severity srcSpan msg
      cntx   = initSDocContext dflags style
  in  error str
