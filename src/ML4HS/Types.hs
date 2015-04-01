module ML4HS.Types (
   Haskell(H)
 , HsFile()
 , mkHs
 , unHs
 , toHs
 , InSession()
 , inSession
 , runInSession
 ) where

import Control.Applicative
import Data.Maybe
import DynFlags
import ErrUtils
import GHC
import GHC.Paths
import GhcMonad
import HscMain
import HscTypes
import Outputable

-- | Haskell code
newtype Haskell = H String

-- | Haskell file paths
newtype HsFile = Hs FilePath  deriving (Show, Eq, Ord)

-- | Turn Strings into HsFiles if they look like Haskell files
mkHs :: FilePath -> Maybe HsFile
mkHs f | take 3 (reverse f) == "sh."  = Just (Hs f)
mkHs f | take 4 (reverse f) == "shl." = Just (Hs f)
mkHs f | otherwise                    = Nothing

-- | Turn a list of FilePaths into a list of HsFiles, discarding invalid ones
toHs :: [FilePath] -> [HsFile]
toHs = catMaybes . map mkHs

-- | Get the path of a Haskell file
unHs :: HsFile -> FilePath
unHs (Hs f) = f

-- Functions using GHC must call 'setSession' before anything else, otherwise
-- "the impossible" will happen (an error will be thrown). The existing GHC
-- types do not distinguish between functions which call 'setSession' themselves
-- and those which assume it has already been called.

-- We introduce 'InSession' to indicate functions which assume 'setSession' has
-- already been called. These functions can be composed arbitrarily, then passed
-- to 'runInSession' which will make the call.

-- NOTE: These types are not watertight, since we can convert back and forth
-- between 'Ghc' and 'IO' using 'runGhc' and 'liftIO'. The intention is to use
-- 'runInSession' instead of 'runGhc', so only one session is used.

-- | Computations which must take place in a GHC session
newtype InSession a = IS (HscEnv -> Ghc a)

-- | Indicate that a 'Ghc' computation requires a session
inSession :: (HscEnv -> Ghc a) -> InSession a
inSession = IS

instance Functor InSession where
  fmap f (IS g) = IS (\env -> fmap f (g env))

instance Applicative InSession where
  pure x = IS (\env -> return x)
  (IS f) <*> (IS g) = IS (\env -> f env <*> g env)

instance Monad InSession where
  return = pure
  (IS f) >>= g = IS (\env -> f env >>= (\x -> case g x of
                                                   IS g' -> g' env))

runInSession :: InSession a -> IO a
runInSession (IS f) = runGhcM f

runGhcM :: (HscEnv -> Ghc a) -> IO a
runGhcM f = runGhc (Just libdir) (inDefaultEnv f)

inDefaultEnv :: (HscEnv -> Ghc a) -> Ghc a
inDefaultEnv f = do dflags <- getSessionDynFlags
                    setSessionDynFlags (dflags { log_action = dieErr })
                    getSession >>= f

-- | Abort GHC when errors are encountered (eg. syntax errors)
dieErr :: LogAction
dieErr dflags severity srcSpan style msg =
  let str    = show (runSDoc locMsg cntx)
      locMsg = mkLocMessage severity srcSpan msg
      cntx   = initSDocContext dflags style
  in  error str
