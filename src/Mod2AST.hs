module Mod2AST where

import Control.Concurrent
import Control.Exception (evaluate)
import Data.List
import HS2AST
import System.Directory
import System.Exit
import System.IO
import System.IO.Temp
import System.Process
import Test.Arbitrary.Cabal
import Test.Arbitrary.Haskell

testMain = putStrLn "hello world"

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
      , ("build-depends", intercalate "," ("HS2AST":"base >= 4.7 && < 5":pkgs))
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

cabal :: [String] -> FilePath -> String -> IO (ExitCode, String, String)
cabal = readProcessWithExitCodeIn "cabal"

-- A specialised version of readProcessWithExitCode, since that doesn't allow
-- setting the working directory :(
readProcessWithExitCodeIn cmd args dir stdin = do
  -- Fire off the process, getting handles back
  (hIn, hOut, hErr, p) <- runInteractiveProcess cmd args (Just dir) Nothing

  -- Read the handles
  out <- hGetContents hOut
  err <- hGetContents hErr

  -- Fork threads to force the whole out/err, using "length"
  outMVar <- newEmptyMVar
  forkIO $ evaluate (length out) >> putMVar outMVar ()
  forkIO $ evaluate (length err) >> putMVar outMVar ()

  -- Send input, if we were given some
  if null stdin then return ()
                else hPutStr hIn stdin >> hFlush hIn
  hClose hIn

  -- Wait for process and threads to complete
  takeMVar outMVar
  takeMVar outMVar
  hClose hOut
  code <- waitForProcess p

  return (code, out, err)

cabal2nix :: FilePath -> IO String
cabal2nix dir = do
  (code, stdout, stderr) <- readProcessWithExitCode "cabal2nix"
                                                    [dir]
                                                    ""
  case code of
       ExitSuccess   -> return stdout
       ExitFailure _ -> do print (("Exit code", code),
                                  ("Stdout", stdout),
                                  ("Stderr", stderr))
                           error "Failed to run cabal2nix"

configureInShell :: FilePath -> IO ()
configureInShell dir = do
  (code, stdout, stderr) <-
    readProcessWithExitCodeIn "nix-shell"
                              ["--run", "cabal configure -v"]
                              dir
                              ""
  case code of
   ExitSuccess -> return ()
   _           -> do print (("Exit code", code),
                            ("stdout", stdout),
                            ("stderr", stderr))
                     error "Failed to configure package"

shellNix dir = unlines [
    "with import <nixpkgs> {};"
  , "let call = haskellPackages.callPackage;"
  , "    pkg  = call ./. {"
  , "             HS2AST = haskell.lib.dontCheck (call " ++ dir ++ " {});"
  , "           };"
  , "in pkg.env"
  ]

writeShellNix dir = do
  cwd     <- getCurrentDirectory
  content <- cabal2nix dir
  writeFile (dir ++ "/default.nix") content
  writeFile (dir ++ "/shell.nix") (shellNix cwd)

configureDeps dir = do
  writeShellNix dir
  configureInShell dir

runMain :: String -> FilePath -> IO (ExitCode, String, String)
runMain stdin dir = do
  configureDeps dir
  cabal ["run", "Mods2Asts"] dir stdin
