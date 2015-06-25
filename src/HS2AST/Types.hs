{-# LANGUAGE DeriveDataTypeable #-}

module HS2AST.Types (
   HsFile()
 , Sexpr(Leaf, Node)
 , mkLeaf
 , mkNode
 , mkHs
 , unHs
 , toHs
 , dummyTypes
 , runInSession
 , runInSessionWith
 , getGhcPkgs
 , haveProg
 , haveNS
 ) where

import Control.Exception (SomeException, try)
import Data.Char
import Data.Data
import Data.Generics.Uniplate.Data
import Data.Maybe
import DynFlags
import ErrUtils
import qualified GHC
import GHC.Paths
import GhcMonad
import Module
import Name
import Outputable
import Packages
import System.Directory
import System.Exit
import System.Process
import TypeRep

-- Haskell files, with smart constructors
-- | Haskell file paths
newtype HsFile = Hs FilePath  deriving (Show, Eq, Ord)

-- | Turn Strings into HsFiles if they look like Haskell files
mkHs :: FilePath -> Maybe HsFile
mkHs f = let ext s = map toLower (take (length s) (reverse f)) == reverse s
         in  if ext ".hs" || ext ".lhs"
                then Just (Hs f)
                else Nothing

-- | Turn a list of FilePaths into a list of HsFiles, discarding invalid ones
toHs :: [FilePath] -> [HsFile]
toHs = mapMaybe mkHs

-- | Get the path of a Haskell file
unHs :: HsFile -> FilePath
unHs (Hs f) = f

-- | Arbitrary rose trees
data Sexpr a = Leaf a | Node [Sexpr a] deriving (Eq, Typeable, Data)

mkLeaf :: Data a => a -> Sexpr a
mkLeaf x = Leaf (dummyTypes x)

mkNode :: Data a => [Sexpr a] -> Sexpr a
mkNode xs = Node (dummyTypes xs)

-- | Replace all Types in an AST with a dummy, to avoid pre-typecheck errors
dummyTypes :: Data a => a -> a
dummyTypes = transformBi (const (LitTy (NumTyLit 0)))

instance Functor Sexpr where
  fmap f (Leaf x)  = Leaf (f x)
  fmap f (Node xs) = Node (map (fmap f) xs)

instance Show a => Show (Sexpr a) where
  show (Leaf x)  = show x
  show (Node xs) = "(" ++ unwords (map show xs) ++ ")"

-- Wrap up the dynamically-dangerous GHC API

-- | Use this to start 'Ghc' computations. It is less flexible than 'runGhc',
-- but avoids session-related runtime errors.
runInSession :: Ghc a -> IO a
runInSession = runInSessionWith []

runInSessionWith pkgs = GHC.runGhc (Just libdir) . inDefaultEnv pkgs

dieErr flg lvl loc fmt msg = error (show (runSDoc (mkLocMessage lvl loc msg)
                                                  (initSDocContext flg fmt)))

pkgDbs = do
  db <- getGhcPkgs
  return (PkgConfFile db)

getGhcPkgs :: IO FilePath
getGhcPkgs = do
  ns <- haveNS
  if ns then do output <- readProcess "nix-shell" ["--run", "ghc-pkg list"] ""
                return (takeWhile (/= ':') (head (lines output)))
        else return ""

haveProg cmd args = do
  found <- try (readProcessWithExitCode
                  cmd
                  args
                  "") :: IO (Either SomeException (ExitCode, String, String))
  case found of
       Left  _ -> print ("Cannot run '" ++ cmd ++ "'") >> return False
       Right _ -> return True

haveNS = haveProg "nix-shell" ["--help"]

getDirs :: IO [FilePath]
getDirs = do
  cwd <- getCurrentDirectory
  out <- readProcess "find" [cwd, "-type", "d"] ""
  return (lines out)

inDefaultEnv pkgs x = do oldFlags <- getSessionDynFlags
                         pkgDb <- liftIO pkgDbs
                         dirs  <- liftIO getDirs
                         let newFlags = oldFlags {
                             log_action   = dieErr
                           , ghcLink      = LinkInMemory
                           , hscTarget    = HscInterpreted
                           -- Include all ghc package's modules, don't rename any
                           , packageFlags = map exposePkg ("ghc":[]{-pkgs-})
                           -- Include this package's dependencies
                           , extraPkgConfs = (pkgDb:)
                           -- Include all directories, just in case (HACK)
                           --, includePaths = dirs  -- C header includes?
                           , importPaths  = dirs
                           --, libraryPaths = dirs
                         }
                         GHC.setSessionDynFlags newFlags
                         liftIO $ initPackages newFlags
                         x

exposePkg p = ExposePackage (PackageArg p) (ModRenaming True [])

-- Helpful instances
instance Show Name where
  show = occNameString . nameOccName

instance Show ModuleName where
  show = moduleNameString

instance Show PackageKey where
  show = packageKeyString
