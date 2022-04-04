{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Kip.AST where

import Data.List

type Identifier = ([String], String)

data Case =
    Nom -- ^ nominative case (yalın hal)
  | Acc -- ^ accusative case (-i hali)
  | Dat -- ^ dative case (-e hali)
  | Loc -- ^ locative case (-de hali)
  | Abl -- ^ ablative case (-den hali)
  | Gen -- ^ genitive case (-in eki)
  | Ins -- ^ instrumental case (ile, -le)
  | Cond -- ^ conditional case (-se, şart kipi)
  deriving (Show, Eq, Ord)

data Ty a =
    TyString { annTy :: a }
  | Arr      { annTy :: a , dom :: Ty a, img :: Ty a }
  | TyInd    { annTy :: a , indName :: Identifier }
  deriving (Show, Eq, Ord)

data Exp a =
    Var    { annExp :: a , varName :: Identifier }
  | App    { annExp :: a , fn :: Exp a , args :: [Exp a] }
  | StrLit { annExp :: a , lit :: String }
  | Let    { annExp :: a , varName :: Identifier , body :: Exp a }
  deriving (Show, Eq)

type Arg ann = (Identifier, Ty ann)
type Ctor ann = (Identifier, [(Identifier, Ty ann)])

data Stmt ann =
    Defn Identifier (Ty ann) (Exp ann)
  | Function Identifier [Arg ann] (Ty ann) (Exp ann)
  | NewType Identifier [Ctor ann]
  | Print (Exp ann)
  | ExpStmt (Exp ann)
  deriving (Show, Eq)

-- data Ty' a =
--     TyString' { annTy' :: a Ty' }
--   | Arr'     { annTy' :: a Ty' , dom' :: Ty' a , img' :: Ty' a }
--   | TyInd'   { annTy' :: a Ty', indName' :: Identifier }
--   deriving (Show, Eq, Ord)

-- type family TypeCheck (k :: (* -> *) -> *) :: * where
--   TypeCheck Ty' = ()

isMultiWord :: String -> Bool
isMultiWord s = length (words s) /= 1

prettyExp :: Exp a -> String
prettyExp (Var _ name) = intercalate "-" (fst name ++ [snd name])
prettyExp (StrLit _ s) = show s
