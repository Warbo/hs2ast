{-# LANGUAGE FlexibleInstances, RankNTypes #-}

module HS2AST.Sexpr where

import           CoAxiom                     as C
import qualified Data.AttoLisp               as L
import           Data.ByteString             (ByteString)
import           Data.ByteString.Char8       (unpack)
import           Data.Char
import           Data.Data
import           Data.Generics
import           Packages
import           FastString
import           Data.Maybe
import qualified Data.Stringable             as S
import           DataCon
import           HS2AST.Types
import           HsBinds
import           Literal
import           Module
import           Name
import           SrcLoc
import           TyCon                       as T
import           TypeRep
import           Unique
import           Var

mkLeaf :: String -> AST
mkLeaf x = L.String (S.fromString . filterLisp $ x)

mkNode :: [AST] -> AST
mkNode = L.List

cleanLisp :: AST -> AST
cleanLisp (L.String x) = mkLeaf (filterLisp (S.toString x))
cleanLisp (L.List xs) = L.List (map cleanLisp xs)

-- | Convert Data instances to s-expressions
toSexp :: Data a => PackageDb -> a -> L.Lisp
toSexp db x = let tail = gmapQ (toSexp db) x
                  head = strConstr db x
               in mkNode (head : tail)

strConstr :: Data a => PackageDb -> a -> L.Lisp
strConstr db = let def = mkLeaf . show . toConstr
                in extQ (extQ (extQ (extQ
                     def
                     showBS)
                     (showVar     db))
                     (showDataCon db))
                     (showTycon   db)

showNamed :: NamedThing a => PackageDb -> String -> a -> L.Lisp
showNamed db s t = let name     = getName t
                       nameTag  = tag "name" (getOccString name)
                       tagMod m = mkNode [nameTag,
                                          tag "mod" (modNameS m),
                                          tag "pkg" (modPkgS  db m)]
                    in mkNode [mkLeaf s,
                               maybe nameTag tagMod (nameModule_maybe name)]

modNameS :: Module -> String
modNameS = moduleNameString . moduleName

modPkgS :: PackageDb -> Module -> String
modPkgS db m = case packageNameFromKey db . modulePackageKey $ m of
  PackageName fs -> unpackFS fs

showTycon :: PackageDb -> T.TyCon -> L.Lisp
showTycon db = showNamed db "TyCon"

showDataCon :: PackageDb -> DataCon -> L.Lisp
showDataCon db = showNamed db "DataCon"

showVar :: PackageDb -> Var -> L.Lisp
showVar db = showNamed db "Var"

tag t x = mkNode [mkLeaf t, mkLeaf x]

contains x           y | x == y = True
contains (L.List xs) y          = any (`contains` y) xs
contains _           _          = False

showBS :: ByteString -> L.Lisp
showBS bs = mkNode [mkLeaf "BS", mkLeaf (unpack bs)]

filterLisp :: String -> String
filterLisp  = filter f
  where f x = isPrint x && ('\\' `notElem` show x)

packageNameFromKey :: PackageDb -> PackageKey -> PackageName
packageNameFromKey db k =
  fromMaybe (PackageName (mkFastString ("HS2AST-NOT-FOUND-" ++ packageKeyString k)))
            (db k)
