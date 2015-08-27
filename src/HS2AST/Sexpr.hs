{-# LANGUAGE FlexibleInstances, RankNTypes #-}

module HS2AST.Sexpr where

import           CoAxiom                     as C
import qualified Data.AttoLisp               as L
import           Data.ByteString             (ByteString)
import           Data.ByteString.Char8       (unpack)
import           Data.Char
import           Data.Data
import           Data.Generics
--import           Data.Generics.Uniplate.Data
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

mkLeaf :: String -> L.Lisp
mkLeaf x = L.String (S.fromString . filterLisp $ x)

mkNode :: [L.Lisp] -> L.Lisp
mkNode = L.List

-- | Convert Data instances to s-expressions
toSexp :: Data a => a -> L.Lisp
toSexp x = let tail = gmapQ toSexp  x
               head = toSx x
            in mkNode (head : tail)

toSx :: Data a =>  a -> L.Lisp
toSx = strConstr

strConstr :: Data a => a -> L.Lisp
strConstr = let def = mkLeaf . show . toConstr
             in extQ (extQ (extQ (extQ def showBS)
                                 showVar)
                           showDataCon)
                     showTycon

showNamed :: NamedThing a => String -> a -> L.Lisp
showNamed s t = let name     = getName t
                    nameTag  = tag "name" (getOccString name)
                    tagMod m = mkNode [nameTag,
                                       tag "mod" (modNameS m),
                                       tag "pkg" (modPkgS  m)]
                 in mkNode [mkLeaf s,
                            maybe nameTag tagMod (nameModule_maybe name)]

modNameS :: Module -> String
modNameS = moduleNameString . moduleName

modPkgS :: Module -> String
modPkgS = packageKeyString . modulePackageKey

showTycon :: T.TyCon -> L.Lisp
showTycon = showNamed "TyCon"

showDataCon :: DataCon -> L.Lisp
showDataCon = showNamed "DataCon"

showVar :: Var -> L.Lisp
showVar = showNamed "Var"

tag t x = mkNode [mkLeaf t, mkLeaf x]

contains x           y | x == y = True
contains (L.List xs) y          = any (`contains` y) xs
contains _           _          = False

showBS :: ByteString -> L.Lisp
showBS bs = mkNode [mkLeaf "BS", mkLeaf (unpack bs)]

filterLisp :: String -> String
filterLisp = filter isPrint
