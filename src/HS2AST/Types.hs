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
 ) where

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
runInSession = GHC.runGhc (Just libdir) . inDefaultEnv

dieErr flg lvl loc fmt msg = error (show (runSDoc (mkLocMessage lvl loc msg)
                                                  (initSDocContext flg fmt)))

pkgDbs = map PkgConfFile ["/home/chris/Programming/Haskell/HS2AST/dist/package.conf.inplace"]

inDefaultEnv x = do oldFlags <- getSessionDynFlags
                    let newFlags = oldFlags {
                        log_action   = dieErr
                      , ghcLink      = LinkInMemory
                      , hscTarget    = HscInterpreted
                      -- Include all ghc package's modules, don't rename any
                      , packageFlags = [ExposePackage (PackageArg "ghc")
                                                      (ModRenaming True [])]
                      -- Include this package's dependencies
                      , extraPkgConfs = \x -> pkgDbs ++ extraPkgConfs oldFlags x
                      }
                    GHC.setSessionDynFlags newFlags
                    liftIO $ initPackages newFlags
                    x

-- Helpful instances
instance Show Name where
  show = occNameString . nameOccName

instance Show ModuleName where
  show = moduleNameString

instance Show PackageKey where
  show = packageKeyString
