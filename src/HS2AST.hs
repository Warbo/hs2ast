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

type Named n a = (n, a)

type OutName = (FilePath, PackageKey, ModuleName, Name)

giveName :: FilePath -> (PackageKey, ModuleName, Name, a) -> Named OutName a
giveName f (a, b, c, d) = ((f, a, b, c), d)

namedAsts f = let f' = unHs f
                  conv (a, b, c, d) = case convertBinding d of
                                           Nothing -> []
                                           Just y  -> [(f', a, b, c, y)]
              in  do bindings <- namedBindingsFrom [f]
                     return $ concatMap conv bindings

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

filesToAsts' :: [HsFile] -> Ghc [Named OutName AST]
filesToAsts' []     = return []
filesToAsts' (f:fs) = let f' = unHs f
                          conv (a, b, c, d, e) = ((a, b, c, d), e)
                      in  do ast  <- namedAsts    f
                             asts <- filesToAsts' fs
                             return (map conv ast ++ asts)

sanitise :: [FilePath] -> [FilePath]
sanitise = filter (not . dodgy)
  where dodgy x = '\n' `elem` x

filesToAsts :: [FilePath] -> IO [String]
filesToAsts fs = do asts <- runInSession (filesToAsts' (toHs (sanitise fs)))
                    return (map showAst asts)

showAst ((a, b, c, d), ast) = name ++ " " ++ show ast
  where bits1 = [a, show b, show c, show d]
        bits2 = map escape bits1
        bits3 = map quote  bits2
        escape [] = []
        escape  ('"':s) = "\\\"" ++ escape s
        escape ('\\':s) = "\\\\" ++ escape s
        escape    (c:s) = c : escape s
        quote s = "\"" ++ s ++ "\""
        name = "(" ++ intercalate " " bits3 ++ ")"
