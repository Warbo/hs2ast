module ML4HS.Types (
   Haskell(H)
 , HsFile()
 , Sexpr(Sx)
 , mkHs
 , unHs
 , toHs
 , runInSession
 , runGhc
 ) where

import Control.Applicative
import Data.Maybe
import DynFlags
import ErrUtils
import Exception
import qualified GHC
import GHC.Paths
import GhcMonad
import HscMain
import HscTypes
import MonadUtils
import Outputable

-- | Haskell code
newtype Haskell = H String

-- | Haskell file paths
newtype HsFile = Hs FilePath  deriving (Show, Eq, Ord)

-- | Turn Strings into HsFiles if they look like Haskell files
mkHs :: FilePath -> Maybe HsFile
mkHs f = let ext s = take (length s) (reverse f) == reverse s
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
data Sexpr a = Sx a [Sexpr a] deriving (Eq)

instance Functor Sexpr where
  fmap f (Sx x xs) = Sx (f x) (map (fmap f) xs)

instance Show a => Show (Sexpr a) where
  show (Sx x xs) = "(" ++ unwords (show x : map show xs) ++ ")"

-- | Use this to start 'Ghc' computations. It is less flexible than 'runGhc',
-- but avoids session-related runtime errors.
runInSession :: Ghc a -> IO a
runInSession = GHC.runGhc (Just libdir) . inDefaultEnv

-- | Tell GHC to abort on errors
setDie = let die f = f { log_action = dieErr }
         in  getSessionDynFlags >>= GHC.setSessionDynFlags . die

-- | Run a GHC computation, aborting on errors
inDefaultEnv :: Ghc a -> Ghc a
inDefaultEnv = (setDie >>)

-- | Abort GHC when errors are encountered (eg. syntax errors)
dieErr :: LogAction
dieErr dflags severity srcSpan style msg =
  let str    = show (runSDoc locMsg cntx)
      locMsg = mkLocMessage severity srcSpan msg
      cntx   = initSDocContext dflags style
  in  error str

-- Clobber runGhc, to make avoiding our API slightly harder

-- | Do not call 'runGHC' directly, use 'runInSession' instead
runGhc :: Maybe FilePath -> Ghc a -> IO a
runGhc = error "Do not call runGHC directly, use runInSession instead"
