module HS2AST.Tests.Mod2AST where

import Control.Exception (SomeException, try)
import Control.Monad
import Data.Char
import Data.List
import Data.List.Utils
import Data.String.Utils
import HS2AST.Types
import Mod2AST
import System.Directory
import System.Exit
import System.Process
import Test.Arbitrary.Cabal
import Test.Arbitrary.Haskell
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run, PropertyM)
import Test.Tasty
import Test.Tasty.QuickCheck

pureTests   = testGroup "Pure Mod2AST tests" [
    testProperty "All packages become build-depends" projectHasPkgs
  , testProperty "Can set main function"             projectHasMain
  ]

impureTests = testGroup "Impure Mod2AST tests" [
    testProperty "Find GHC package path"                findGhcPath
  , testProperty "HS2AST dependencies in HS2AST pkg DB" dependenciesInDb
  --, testProperty "Can add HS2AST as a dependency"       canAddHS2ASTDependency  SEE FIXME
  , testProperty "Actions are run with project dir"     actionsAreRun
  , testProperty "Temporary Cabal directory made"       projectDirMade
  , testProperty "Temporary Cabal directory cleaned up" projectDirCleanedUp
  , testProperty "Temporary Cabal project is valid"     projectIsValidCabal
  , testProperty "Can execute Main.hs"                  projectRunHelloWorld
  ]

projectHasPkgs pkgs = all (`elem` deps) pkgs'
  where pkgs'       = filter (\x -> all isAlpha x && x /= "") pkgs
        project     = genProject pkgs' "testMain"
        S _ exec    = head (sections project)
        Just depStr = lookup "build-depends" exec
        deps        = split "," depStr

projectHasMain main = ("main = " ++ main') `elem` lines mainHs
  where main'           = filter (/= '\n') main
        Just (H mainHs) = lookup ([], "Main.hs") (files (genProject [] main'))

actionsAreRun :: String -> Int -> Property
actionsAreRun main expected = monadicIO $ do
  result <- run $ mkCabalIn "hs2asttest" [] main (const (return expected))
  assert (result == expected)

projectDirMade main = monadicIO $ do
  exists <- run $ mkCabalIn "hs2asttest" [] main doesDirectoryExist
  assert exists

projectDirCleanedUp main = monadicIO $ do
  dir <- run $ mkCabalIn "hs2asttest" [] main return
  exists <- run $ doesDirectoryExist dir
  assert (not exists)

projectIsValidCabal main = monadicIO $ do
  valid <- run $ mkCabalIn "hs2asttest" [] main cabalCheck
  assert valid

projectRunHelloWorld = monadicIO $ do
  (ran, code, out, err) <- run $ mkCabalIn "hs2asttest" [] "testMain" checkMain
  let msg = (("Exit code", code), ("Stdout", out), ("Stderr", err))
  when ran $ do assertMsg (code == ExitSuccess)            msg
                assertMsg ("hello world" `elem` lines out) msg

  where checkMain = runIfC2N ""

extractAstsFromBase = monadicIO $ do
  (ran, code, out, err) <- run $ mkCabalIn "hs2asttest" [] "mod2Ast" doMain
  let msg = (("Exit code", code), ("Stdout", out), ("Stderr", err))
  run $ print msg
  --when ran $ do assertMsg (code == ExitSuccess) msg
  where doMain = runIfC2N (unlines ["Data.List", "Data.Maybe"])

dependenciesInDb = monadicIO $ do
  ns <- run haveNS
  when ns $ do deps <- run hs2astDeps
               db   <- run getGhcPkgs
               pkgs <- run $ getDirectoryContents db
               mapM_ (inDb pkgs) deps
  where inDb []     dep                      = assertMsg False ("dep", dep)
        inDb (p:ps) dep | dep `isPrefixOf` p = return ()
        inDb (p:ps) dep                      = inDb ps dep

findGhcPath = monadicIO $ do
  ns <- run haveNS
  when ns $ do path <- run getGhcPkgs
               assertMsg ("/nix/store" `isPrefixOf` path) ("path", path)

-- FIXME: Hangs the test suite when we get to readFile
canAddHS2ASTDependency = monadicIO $ do
  ns <- run haveNS
  when ns $ do result <- run $ mkCabalIn "hs2asttest" [] "testMain" alterDeps
               assert False
  where alterDeps dir = do
          contents <- getDirectoryContents dir
          let cblFile' = head (filter (".cabal" `isSuffixOf`) contents)
              cblFile  = dir ++ "/" ++ cblFile
          cbl <- readFile cblFile
          let deps = extractCabalField "build-depends" cbl
          alterDeps cblFile
          newCbl <- readFile cblFile
          putStrLn "OLD"
          putStrLn cbl
          putStrLn "NEW"
          putStrLn newCbl
          return True

-- Helpers

runIfC2N stdin dir = do
  c2n <- haveC2N
  if c2n then do (code, out, err) <- runMain stdin dir
                 return (True, code, out, err)
         else do print "No cabal2nix, skipping test"
                 return (False, undefined, undefined, undefined)

debug :: Show a => Bool -> a -> PropertyM IO ()
debug b x = run $ when b (print x)

assertMsg b a = do run $ when (not b) $ print a
                   assert b

haveCabal = haveProg "cabal" ["--help"]

haveC2N = haveProg "cabal2nix" ["--help"]

cabalCheck :: FilePath -> IO Bool
cabalCheck dir = do
  have <- haveCabal
  if have then do (code, sOut, sErr) <- cabal ["check"] dir ""
                  case code of
                       ExitSuccess   -> return True
                       ExitFailure c -> do print (("Exit code", c),
                                                  ("Stdout", sOut),
                                                  ("Stderr", sErr))
                                           return False
          else print "Cabal not available" >> return True

hs2astCabal = readFile "HS2AST.cabal"
hs2astDeps  = fmap (extractCabalField "build-depends")   hs2astCabal
hs2astMods  = fmap (extractCabalField "exposed-modules") hs2astCabal
