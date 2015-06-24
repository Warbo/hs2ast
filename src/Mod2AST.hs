module Mod2AST where

import Control.Concurrent
import Control.Exception (evaluate)
import Data.List
import HS2AST
import System.Exit
import System.IO
import System.IO.Temp
import System.Process
import Test.Arbitrary.Cabal
import Test.Arbitrary.Haskell

testMain = print "hello world"

           -- Get our input modules from stdin
mod2Ast = do input <- getContents

             -- Build ASTs
             result <- modsToAsts (lines input)

             -- Print results to stdout
             putStr (unlines result)

modsToAsts = return . map reverse

mkCabal = mkCabalIn "hs2ast"

--mkCabal :: IO a
mkCabalIn dir pkgs main next = withSystemTempDirectory dir (useCabal pkgs main next)

--useCabal :: FilePath -> IO ()
useCabal pkgs main next dir = makeProject dir (genProject pkgs main) >>= next

genProject pkgs main = P {
    name = "hs2astTemp"
  , version = [1]
  , headers  = S () [
      ("cabal-version", ">= 1.8")
    , ("build-type",    "Simple")
    , ("category",      "Language")
    , ("maintainer",    "nobody@example.com")
    , ("description",   "Temporary pkg")
    , ("synopsis",      "Temporary pkg")
    , ("license",       "GPL")
    , ("license-file",  "LICENSE")
    ]
  , sections = [
    S "executable Mods2Asts" [
        ("main-is", "Main.hs")
      , ("build-depends", intercalate "," (pkgs ++ hs2astDeps))
      ]
    ]
  , files = [
      (([], "Main.hs"), H (unlines [
        "module Main where",
        "import Mod2AST",
        "main = " ++ main
      ]))
    ]
  }

hs2astDeps = [ "base >= 4.7 && < 5"
             , "ghc"
             , "ghc-paths"
             , "QuickCheck"
             , "uniplate"
             , "transformers"
             , "syb"
             , "directory"
             , "bytestring"
             , "containers"
             , "ArbitraryHaskell"
             , "temporary"
             ]

-- A specialised version of readProcessWithExitCode, since that doesn't allow
-- setting the working directory :(
cabal :: [String] -> FilePath -> String -> IO (ExitCode, String, String)
cabal args dir sin = do
  -- Fire off the process, getting handles back
  (hIn, hOut, hErr, p) <- runInteractiveProcess "cabal" args (Just dir) Nothing

  -- Read the handles
  out <- hGetContents hOut
  err <- hGetContents hErr

  -- Fork threads to force the whole out/err, using "length"
  outMVar <- newEmptyMVar
  forkIO $ evaluate (length out) >> putMVar outMVar ()
  forkIO $ evaluate (length err) >> putMVar outMVar ()

  -- Send input, if we were given some
  if null sin then return ()
              else hPutStr hIn sin >> hFlush hIn
  hClose hIn

  -- Wait for process and threads to complete
  takeMVar outMVar
  takeMVar outMVar
  hClose hOut
  code <- waitForProcess p

  return (code, out, err)
