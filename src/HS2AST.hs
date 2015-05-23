module HS2AST where

import           Control.Applicative
import           Data.List
import           Data.Maybe
import qualified Data.Map as DM
import           GhcMonad
import           Module
import           Name
import           HS2AST.Parser
import           HS2AST.Sexpr
import           HS2AST.Types
import           System.Directory

getAsts :: [HsFile] -> Ghc (Sexpr String)
getAsts fs = do bindings <- namedBindingsFrom fs
                return (collateBindings bindings)

identifiedAsts :: [HsFile] -> Ghc (Sexpr String)
identifiedAsts fs = do bindings <- namedBindingsFrom fs
                       return (collateIdentified (identifyAsts bindings))

renderAsts :: Sexpr String -> String
renderAsts = show

type Dir  = FilePath
type File = FilePath

mkDir = createDirectoryIfMissing True

leafList (Node [])            = []
leafList (Node (Leaf x : xs)) = x : leafList (Node xs)

-- | Given an Sexpr describing an AST, returns a list of directories we need to
--   create, along with file/content pairs we should write
filesToWrite :: Sexpr String -> (Dir, (File, String))
filesToWrite (Node [ids, ast]) = let path = leafList ids
                                 in  ( intercalate "/" (init path),
                                      (intercalate "/"       path, show ast))

collateFiles :: [(Dir, (File, String))] -> ([Dir], [(File, String)])
collateFiles = cF ([], [])

{-
collateFiles []            = ([], [])
collateFiles ((d, f):xs) = let (ds, fs) = collateFiles xs
                           in  (d:ds, f:fs)
-}

cF acc      []          = acc
cF (ds, fs) ((d, f):xs) = cF (d:ds, f:fs) xs

writeAsts' :: Dir -> [Dir] -> [(File, String)] -> IO ()
writeAsts' dir ds fs = let inDir f = dir ++ "/" ++ f
                           write (f, s) = writeFile (inDir f) s
                       in  mapM (mkDir . inDir) ds >>
                           mapM write fs >>
                           return ()

writeAsts :: Dir -> Sexpr String -> IO ()
writeAsts dir (Node xs) = uncurry (writeAsts' dir) . collateFiles . map filesToWrite $ xs
