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

-- | Check if a type is a float
isFloatTy :: Ty Ann -> Bool
isFloatTy (TyFloat _) = True
isFloatTy _ = False

-- | Check if a type is a string
isStringTy :: Ty Ann -> Bool
isStringTy (TyString _) = True
isStringTy _ = False

-- | All known primitive functions
allPrimitives :: [PrimitiveDef]
allPrimitives =
  [ PrimitiveDef ([], "yaz")
      [ withTypes 1 (\case [t] -> isIntTy t || isFloatTy t || isStringTy t; _ -> False)
      , anyTypes 2  -- File write
      ]
      ["temel-etki.kip"]

  , PrimitiveDef ([], "oku")
      [ anyTypes 0  -- stdin
      , withTypes 1 (\case [t] -> isStringTy t; _ -> False)  -- file read
      ]
      ["temel-etki.kip"]

  , PrimitiveDef ([], "uzunluk")
      [ anyTypes 1 ]
      ["temel-dizge.kip"]

  , PrimitiveDef ([], "birleşim")
      [ anyTypes 2 ]
      ["temel-dizge.kip"]

  , PrimitiveDef (["tam", "sayı"], "hal")
      [ anyTypes 1 ]
      ["temel-dizge.kip"]

  , PrimitiveDef (["ondalık", "sayı"], "hal")
      [ anyTypes 1 ]
      ["temel-dizge.kip"]

  , PrimitiveDef ([], "ters")
      [ withTypes 1 (\case [t] -> isStringTy t; _ -> False) ]
      ["temel-dizge.kip"]

  , PrimitiveDef ([], "toplam")
      [ withTypes 2 (\case [t1, t2] -> isFloatTy t1 || isFloatTy t2; _ -> False)
      , withTypes 2 (\case [t1, t2] -> isIntTy t1 && isIntTy t2; _ -> False)
      ]
      ["temel-tam-sayı.kip", "temel-ondalık-sayı.kip"]

  , PrimitiveDef ([], "çarpım")
      [ withTypes 2 (\case [t1, t2] -> isFloatTy t1 || isFloatTy t2; _ -> False)
      , withTypes 2 (\case [t1, t2] -> isIntTy t1 && isIntTy t2; _ -> False)
      ]
      ["temel-tam-sayı.kip", "temel-ondalık-sayı.kip"]

  , PrimitiveDef ([], "fark")
      [ withTypes 2 (\case [t1, t2] -> isFloatTy t1 || isFloatTy t2; _ -> False)
      , withTypes 2 (\case [t1, t2] -> isIntTy t1 && isIntTy t2; _ -> False)
      ]
      ["temel-tam-sayı.kip", "temel-ondalık-sayı.kip"]

  , PrimitiveDef ([], "bölüm")
      [ withTypes 2 (\case [t1, t2] -> isFloatTy t1 || isFloatTy t2; _ -> False)
      , withTypes 2 (\case [t1, t2] -> isIntTy t1 && isIntTy t2; _ -> False)
      ]
      ["temel-tam-sayı.kip", "temel-ondalık-sayı.kip"]

  , PrimitiveDef ([], "kalan")
      [ withTypes 2 (\case [t1, t2] -> isFloatTy t1 || isFloatTy t2; _ -> False)
      , withTypes 2 (\case [t1, t2] -> isIntTy t1 && isIntTy t2; _ -> False)
      ]
      ["temel-tam-sayı.kip", "temel-ondalık-sayı.kip"]

  , PrimitiveDef (["dizge"], "hal")
      [ withTypes 1 (\case [t] -> isFloatTy t || isIntTy t; _ -> False) ]
      ["temel-tam-sayı.kip", "temel-ondalık-sayı.kip"]

  , PrimitiveDef ([], "eşitlik")
      [ withTypes 2 (\case [t1, t2] -> (isFloatTy t1 || isFloatTy t2) || (isIntTy t1 && isIntTy t2); _ -> False) ]
      ["temel-tam-sayı.kip", "temel-ondalık-sayı.kip"]

  , PrimitiveDef ([], "küçüklük")
      [ withTypes 2 (\case [t1, t2] -> (isFloatTy t1 || isFloatTy t2) || (isIntTy t1 && isIntTy t2); _ -> False) ]
      ["temel-tam-sayı.kip", "temel-ondalık-sayı.kip"]

  , PrimitiveDef (["küçük"], "eşitlik")
      [ withTypes 2 (\case [t1, t2] -> (isFloatTy t1 || isFloatTy t2) || (isIntTy t1 && isIntTy t2); _ -> False) ]
      ["temel-tam-sayı.kip", "temel-ondalık-sayı.kip"]

  , PrimitiveDef ([], "büyüklük")
      [ withTypes 2 (\case [t1, t2] -> (isFloatTy t1 || isFloatTy t2) || (isIntTy t1 && isIntTy t2); _ -> False) ]
      ["temel-tam-sayı.kip", "temel-ondalık-sayı.kip"]

  , PrimitiveDef (["büyük"], "eşitlik")
      [ withTypes 2 (\case [t1, t2] -> (isFloatTy t1 || isFloatTy t2) || (isIntTy t1 && isIntTy t2); _ -> False) ]
      ["temel-tam-sayı.kip", "temel-ondalık-sayı.kip"]

  , PrimitiveDef (["sayı"], "çek")
      [ anyTypes 2 ]
      ["temel-etki.kip"]

  , PrimitiveDef ([], "dur")
      [ anyTypes 0 ]
      []
  ]

-- | Check if a primitive function signature is implemented
isImplementedPrimitive :: Identifier -> [Arg Ann] -> Bool
isImplementedPrimitive name args =
  let numArgs = length args
      matchingPrims = filter (\p -> primId p == name) allPrimitives
  in case matchingPrims of
       [] -> False
       (prim:_) ->
         any (\variant ->
           variantArity variant == numArgs &&
           variantArgTypeCheck variant args
         ) (primVariants prim)

-- | Map a primitive identifier to the files that define it
primFiles :: Identifier -> [FilePath]
primFiles name =
  case filter (\p -> primId p == name) allPrimitives of
    [] -> []
    (prim:_) -> primSourceFiles prim
