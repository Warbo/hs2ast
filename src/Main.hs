{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Main where

import GHC
import GHC.Paths
import HsDecls
import HscMain
import HscTypes
import Control.Applicative
import Tip
import Tip.HaskellFrontend
import Tip.Params

defaultEnv :: IO HscEnv
defaultEnv = do dflags <- runGhc Nothing getSessionDynFlags
                newHscEnv dflags

renameMod ms = do env          <- defaultEnv
                  pm           <- hscParse env ms
                  (_, renamed) <- hscTypecheckRename env ms pm
                  case renamed of
                       Nothing            -> error "Did not rename"
                       Just (x, _, _, _)  -> return (defs x)

defs :: HsGroup Name -> HsValBinds Name
defs g = hs_valds g

graphMods fs = do
         getSessionDynFlags >>= setSessionDynFlags
         mapM (`guessTarget` Nothing) fs >>= setTargets
         load LoadAllTargets
         depanal [] True

main = runGhc (Just libdir) $ do
         mods <- graphMods ["tests/data/arith.hs"]
         map renameMod mods
