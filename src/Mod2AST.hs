module Mod2AST where

import Control.Concurrent
import Control.Exception (evaluate)
import Control.Monad
import Data.List
import Data.List.Utils
import Data.String.Utils
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.PrettyPrint
import Distribution.Verbosity
import Distribution.Version
import GHC
import HS2AST
import HS2AST.Parser
import HS2AST.Sexpr
import HS2AST.Types
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

modsToAsts = modsToAstsWith []

modsToAstsWith :: [String] -> [String] -> IO [String]
modsToAstsWith pkgs mods = do asts <- runInSessionWith pkgs (modsToAsts' mods mods)
                              return (map showAst asts)

modsToAsts' :: [String] -> [String] -> Ghc [Named OutName AST]
modsToAsts' mods []     = return []
modsToAsts' mods (m:ms) = do ast  <- namedAstsM  mods m
                             asts <- modsToAsts' mods ms
                             return (ast ++ asts)

namedAstsM mods m = do bindings <- namedBindingsFromM mods m
                       return $ concatMap convertToNamed bindings

nB2 (a, b, c, [])   = []
nB2 (a, b, c, d:ds) = ((a, b, c), d) : nB2 (a, b, c, ds)

namedBindingsFromM mods m = do bindings <- bindingsFromM mods m
                               return (concatMap namedBinding (concatMap nB2 bindings))

bindingsFromM mods m = do sumss <- graphAllMods [m] mods
                          --IIDecl (simpleImportDecl (mkModuleName m))
                          --  ]
                          let sums = concat sumss
                          mapM (renameMod "") sums

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

{-
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
-}

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
  when (code /= ExitSuccess) $ do
    print (("Exit code", code),
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

extractCabalField fld cbl =
  nub $ concatMap (map stripVersion . split "," . unlines)
                  (extractCabalField' fld (lines cbl))

extractCabalField' fld cbl = if null pre
                                then []
                                else (fst:rst) : extractCabalField' fld (tail pre)
  where fld' = fld ++ ":"
        -- Remove everything before the first occurence of this field
        pre  = dropWhile (not . (fld' `isInfixOf`)) cbl
        -- Remove everything up to and including the ':'
        fst  = tail . dropWhile (/= ':') . head $ pre
        -- Keep any subsequent lines, until we find one containing ':'
        rst  = takeWhile (not . stop) . tail $ pre
        stop l = let l' = strip l
                 in ':'           `elem`       l' ||
                    "test-suite " `isPrefixOf` l' ||
                    "executable " `isPrefixOf` l' ||
                    "library "    `isPrefixOf` l'

stripVersion x = takeWhile (/= ' ') (strip x)

alterCabal file func = do
  pkg <- readPackageDescription silent file
  writeGenericPackageDescription file (func pkg)

alterDeps file = alterCabal file addHS2ASTDep

addHS2ASTDep :: GenericPackageDescription -> GenericPackageDescription
addHS2ASTDep p = p {
    condLibrary     = fmap addToLib   (condLibrary     p)
  , condExecutables = fmap addToNamed   (condExecutables p)
  , condTestSuites  = fmap addToNamed  (condTestSuites  p)
  , condBenchmarks  = fmap addToNamed (condBenchmarks p)
  }
  where addToNamed (s, t) = (s, addToLib t)
        addToLib       t  = t {
            condTreeConstraints = hs2ast : condTreeConstraints t
          }
        hs2ast = Dependency (PackageName "HS2AST") anyVersion
