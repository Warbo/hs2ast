module Main
       ( module Main
       ) where

import Control.Monad.IO.Class
import Control.Applicative
import Digraph
import Unsafe.Coerce
import           GHC
import Module
import           GHC.Paths ( libdir )
import           DynFlags
import Outputable

--inGhc = do getSessionDynFlags >>= setSessionDynFlags
--           t <- guessTarger

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

withArith f = inSession $ do
       target <- guessTarget "tests/data/arith.hs" Nothing
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
