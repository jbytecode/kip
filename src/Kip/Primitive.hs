{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{- | Central registry for primitive functions.

This module provides a single source of truth for all primitive functions in Kip.
Different modules can annotate primitives with their specific information:
- TypeCheck: type checking logic
- Eval: runtime implementation
- Codegen.JS: JavaScript code generation
-}
module Kip.Primitive
  ( PrimitiveDef(..)
  , PrimitiveVariant(..)
  , allPrimitives
  , isImplementedPrimitive
  , primFiles
  ) where

import qualified Data.Text as T
import Data.Text (Text)
import Kip.AST (Identifier, Ty(..), Arg, Ann)

-- | A primitive function definition with its variants
data PrimitiveDef = PrimitiveDef
  { primId :: Identifier
    -- ^ The identifier (mods, name) for this primitive
  , primVariants :: [PrimitiveVariant]
    -- ^ Different overloaded variants of this primitive
  , primSourceFiles :: [FilePath]
    -- ^ Library files where this primitive is declared
  } deriving (Show, Eq)

-- | A variant of a primitive function (for overloading)
data PrimitiveVariant = PrimitiveVariant
  { variantArity :: Int
    -- ^ Number of arguments for this variant
  , variantArgTypeCheck :: [Arg Ann] -> Bool
    -- ^ Predicate to check if argument types match this variant
  }

instance Show PrimitiveVariant where
  show v = "PrimitiveVariant { arity = " ++ show (variantArity v) ++ " }"

instance Eq PrimitiveVariant where
  v1 == v2 = variantArity v1 == variantArity v2

-- | Helper to create a variant that accepts any types
anyTypes :: Int -> PrimitiveVariant
anyTypes n = PrimitiveVariant n (const True)

-- | Helper to create a variant that checks specific type constructors
withTypes :: Int -> ([Ty Ann] -> Bool) -> PrimitiveVariant
withTypes n check = PrimitiveVariant n (check . map snd)

-- | Check if a type is an integer
isIntTy :: Ty Ann -> Bool
isIntTy (TyInt _) = True
isIntTy _ = False

-- | Check for the integer type identifier.
isIntIdent :: Identifier -> Bool
isIntIdent (mods, name) = mods == [T.pack "tam"] && name == T.pack "sayı"

-- | Check if a type is a float
isFloatTy :: Ty Ann -> Bool
isFloatTy (TyFloat _) = True
isFloatTy _ = False

-- | Check for the floating-point type identifier.
isFloatIdent :: Identifier -> Bool
isFloatIdent (mods, name) = mods == [T.pack "ondalık"] && name == T.pack "sayı"

-- | Check if a type is a string
isStringTy :: Ty Ann -> Bool
isStringTy (TyString _) = True
isStringTy _ = False

-- | Check for the string type identifier.
isStringIdent :: Identifier -> Bool
isStringIdent (mods, name) = null mods && name == T.pack "dizge"

-- | Normalize primitive aliases to canonical primitive constructors.
normalizePrimTy :: Ty Ann -> Ty Ann
normalizePrimTy ty =
  case ty of
    TyInd ann name
      | isIntIdent name -> TyInt ann
      | isFloatIdent name -> TyFloat ann
      | isStringIdent name -> TyString ann
      | otherwise -> TyInd ann name
    TyVar ann name
      | isIntIdent name -> TyInt ann
      | isFloatIdent name -> TyFloat ann
      | isStringIdent name -> TyString ann
      | otherwise -> TyVar ann name
    TyApp ann ctor args ->
      TyApp ann (normalizePrimTy ctor) (map normalizePrimTy args)
    Arr ann d i ->
      Arr ann (normalizePrimTy d) (normalizePrimTy i)
    TySkolem ann name ->
      TySkolem ann name
    _ -> ty

-- | Check whether a type still contains unresolved type variables.
containsTyVar :: Ty Ann -> Bool
containsTyVar ty =
  case ty of
    TyVar {} -> True
    TyApp _ ctor args -> containsTyVar ctor || any containsTyVar args
    Arr _ d i -> containsTyVar d || containsTyVar i
    _ -> False

-- | All known primitive functions
allPrimitives :: [PrimitiveDef]
allPrimitives =
  [ PrimitiveDef ([], "yaz")
      [ withTypes 1 (\case [t] -> isIntTy t || isFloatTy t || isStringTy t; _ -> False)
      , anyTypes 2  -- File write
      ]
      ["etki.kip"]

  , PrimitiveDef ([], "oku")
      [ anyTypes 0  -- stdin
      , withTypes 1 (\case [t] -> isStringTy t; _ -> False)  -- file read
      ]
      ["etki.kip"]

  , PrimitiveDef ([], "uzunluk")
      [ anyTypes 1 ]
      ["dizge.kip"]

  , PrimitiveDef ([], "birleşim")
      [ anyTypes 2 ]
      ["dizge.kip"]

  , PrimitiveDef (["tam", "sayı"], "hal")
      [ anyTypes 1 ]
      ["dizge.kip"]

  , PrimitiveDef (["ondalık", "sayı"], "hal")
      [ anyTypes 1 ]
      ["dizge.kip"]

  , PrimitiveDef ([], "ters")
      [ withTypes 1 (\case [t] -> isStringTy t; _ -> False) ]
      ["dizge.kip"]

  , PrimitiveDef ([], "toplam")
      [ withTypes 2 (\case [t1, t2] -> isFloatTy t1 || isFloatTy t2; _ -> False)
      , withTypes 2 (\case [t1, t2] -> isIntTy t1 && isIntTy t2; _ -> False)
      ]
      ["tam-sayı.kip", "ondalık-sayı.kip"]

  , PrimitiveDef ([], "çarpım")
      [ withTypes 2 (\case [t1, t2] -> isFloatTy t1 || isFloatTy t2; _ -> False)
      , withTypes 2 (\case [t1, t2] -> isIntTy t1 && isIntTy t2; _ -> False)
      ]
      ["tam-sayı.kip", "ondalık-sayı.kip"]

  , PrimitiveDef ([], "fark")
      [ withTypes 2 (\case [t1, t2] -> isFloatTy t1 || isFloatTy t2; _ -> False)
      , withTypes 2 (\case [t1, t2] -> isIntTy t1 && isIntTy t2; _ -> False)
      ]
      ["tam-sayı.kip", "ondalık-sayı.kip"]

  , PrimitiveDef ([], "bölüm")
      [ withTypes 2 (\case [t1, t2] -> isFloatTy t1 || isFloatTy t2; _ -> False)
      , withTypes 2 (\case [t1, t2] -> isIntTy t1 && isIntTy t2; _ -> False)
      ]
      ["tam-sayı.kip", "ondalık-sayı.kip"]

  , PrimitiveDef ([], "kalan")
      [ withTypes 2 (\case [t1, t2] -> isFloatTy t1 || isFloatTy t2; _ -> False)
      , withTypes 2 (\case [t1, t2] -> isIntTy t1 && isIntTy t2; _ -> False)
      ]
      ["tam-sayı.kip", "ondalık-sayı.kip"]

  , PrimitiveDef (["dizge"], "hal")
      [ withTypes 1 (\case [t] -> isFloatTy t || isIntTy t; _ -> False) ]
      ["tam-sayı.kip", "ondalık-sayı.kip"]

  , PrimitiveDef ([], "eşitlik")
      [ withTypes 2 (\case [t1, t2] -> (isFloatTy t1 || isFloatTy t2) || (isIntTy t1 && isIntTy t2); _ -> False) ]
      ["tam-sayı.kip", "ondalık-sayı.kip"]

  , PrimitiveDef ([], "küçüklük")
      [ withTypes 2 (\case [t1, t2] -> (isFloatTy t1 || isFloatTy t2) || (isIntTy t1 && isIntTy t2); _ -> False) ]
      ["tam-sayı.kip", "ondalık-sayı.kip"]

  , PrimitiveDef (["küçük"], "eşitlik")
      [ withTypes 2 (\case [t1, t2] -> (isFloatTy t1 || isFloatTy t2) || (isIntTy t1 && isIntTy t2); _ -> False) ]
      ["tam-sayı.kip", "ondalık-sayı.kip"]

  , PrimitiveDef ([], "büyüklük")
      [ withTypes 2 (\case [t1, t2] -> (isFloatTy t1 || isFloatTy t2) || (isIntTy t1 && isIntTy t2); _ -> False) ]
      ["tam-sayı.kip", "ondalık-sayı.kip"]

  , PrimitiveDef (["büyük"], "eşitlik")
      [ withTypes 2 (\case [t1, t2] -> (isFloatTy t1 || isFloatTy t2) || (isIntTy t1 && isIntTy t2); _ -> False) ]
      ["tam-sayı.kip", "ondalık-sayı.kip"]

  , PrimitiveDef (["sayı"], "çek")
      [ anyTypes 2 ]
      ["etki.kip"]

  , PrimitiveDef ([], "dur")
      [ anyTypes 0 ]
      []
  ]

-- | Check if a primitive function signature is implemented
isImplementedPrimitive :: Identifier -> [Arg Ann] -> Bool
isImplementedPrimitive name args =
  let normalizedArgs = map (\(arg, ty) -> (arg, normalizePrimTy ty)) args
      numArgs = length normalizedArgs
      matchingPrims = filter (\p -> primId p == name) allPrimitives
      hasUnknownTyVar = any (containsTyVar . snd) normalizedArgs
  in case matchingPrims of
       [] -> False
       (prim:_) ->
         let typedMatch =
               any (\variant ->
                 variantArity variant == numArgs &&
                 variantArgTypeCheck variant normalizedArgs
               ) (primVariants prim)
             arityOnlyMatch =
               any (\variant -> variantArity variant == numArgs) (primVariants prim)
         in typedMatch || (hasUnknownTyVar && arityOnlyMatch)

-- | Map a primitive identifier to the files that define it
primFiles :: Identifier -> [FilePath]
primFiles name =
  case filter (\p -> primId p == name) allPrimitives of
    [] -> []
    (prim:_) -> primSourceFiles prim
