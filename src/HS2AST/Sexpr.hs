{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE RankNTypes         #-}

module HS2AST.Sexpr where

import           CoAxiom                     as C
import qualified Data.AttoLisp               as L
import           Data.ByteString             (ByteString)
import           Data.ByteString.Char8       (unpack)
import           Data.Data
import           Data.Generics
import           Data.Generics.Uniplate.Data
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
mkLeaf x = L.String (S.fromString x)

mkNode :: [L.Lisp] -> L.Lisp
mkNode = L.List

convertBinding :: HsBindLR Name Name -> L.Lisp
convertBinding = toSexp

-- | Convert Data instances to s-expressions
toSexp :: Data a => a -> L.Lisp
toSexp x = let tail = gmapQ toSexp  x
               head = toSx x
            in mkNode (head : tail)

toSx :: Data a =>  a -> L.Lisp
toSx = strConstr

simpleAst :: Data a => a -> L.Lisp
simpleAst = toSexp

strConstr :: Data a => a -> L.Lisp
strConstr = let def = mkLeaf . show . toConstr
             in extQ (extQ (extQ (extQ def showBS)
                                 showVar)
                           showDataCon)
                     showTycon

showNamed :: NamedThing a => String -> a -> L.Lisp
showNamed s t = let name    = getName t
                    mdpkg   = getModPkg (nameModule_maybe name)
                    nameTag = tag "name" (getOccString name)
                 in mkNode [mkLeaf s,
                            case mdpkg of
                                 Just (m, p) -> mkNode [nameTag,
                                                        tag "mod" m,
                                                        tag "pkg" p]
                                 Nothing     -> nameTag]

showTycon :: T.TyCon -> L.Lisp
showTycon = showNamed "TyCon"

showDataCon :: DataCon -> L.Lisp
showDataCon = showNamed "DataCon"

showVar :: Var -> L.Lisp
showVar = showNamed "Var"

tag t x = mkNode [mkLeaf t, mkLeaf x]

getModPkg :: Maybe Module -> Maybe (String, String)
getModPkg Nothing = Nothing
getModPkg (Just m) = Just (moduleNameString (moduleName m), packageKeyString  (modulePackageKey m))

showBS :: ByteString -> L.Lisp
showBS bs = mkNode [mkLeaf "BS", mkLeaf (unpack bs)]
