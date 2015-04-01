module ML4HS.Types (
   Haskell(H)
 , HsFile()
 , mkHs
 , unHs
 ) where

-- | Haskell code
newtype Haskell = H String

-- | Haskell file paths
newtype HsFile = Hs FilePath  deriving (Show, Eq, Ord)

-- | Turn Strings into HsFiles if they look like Haskell files
mkHs :: String -> Maybe HsFile
mkHs f | take 3 (reverse f) == "sh."  = Just (Hs f)
mkHs f | take 4 (reverse f) == "shl." = Just (Hs f)
mkHs f | otherwise                    = Nothing

-- | Get the path of a Haskell file
unHs :: HsFile -> FilePath
unHs (Hs f) = f
