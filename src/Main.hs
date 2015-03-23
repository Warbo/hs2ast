{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Main where

import Control.Monad.IO.Class
import Control.Applicative
import Digraph
import           GHC
--import Module
import           GHC.Paths ( libdir )
import           DynFlags
import Outputable

-- Run a GHC function in a default session
inSession :: Ghc a -> IO a
inSession f = defaultErrorHandler defaultFatalMessager defaultFlushOut $
  runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    setSessionDynFlags $ dflags { hscTarget = HscNothing }
    f

getSrc = withArith $ do
               modSum <- getModSummary $ mkModuleName "Arith"
               p <- parseModule modSum
               t <- typecheckModule p
               d <- desugarModule t
               l <- loadModule d
               n <- getNamesInScope
               c <- return $ coreModule d
               g <- getModuleGraph
               mapM showModule g
               return $ (parsedSource d,"/n-----/n",  typecheckedSource d)

showSrc = do res <- getSrc
             str <- runGhc (Just libdir) $ do
                      dflags <- getSessionDynFlags
                      return $ showSDoc dflags $ ppr res
             putStrLn str

--namesIn path = withTarget path

withArith = withTarget "tests/data/arith.hs"

withTarget path f = inSession $ do
  target <- guessTarget path Nothing
  addTarget target
  r <- load LoadAllTargets
  case r of
       Failed    -> error ("FAILED")
       Succeeded -> f

graph = do liftIO $ print "HI"
           graph <- depanal [] False
           liftIO $ print "2"
           return $ flattenSCCs (topSortModuleGraph False graph Nothing)

main = withArith graph
