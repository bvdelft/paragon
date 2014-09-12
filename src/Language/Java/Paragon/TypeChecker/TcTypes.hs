{-# LANGUAGE DeriveDataTypeable #-}
module Language.Java.Paragon.TypeChecker.TcTypes 
  (
    -- * Default for type-less nodes
    noTypeAnn
    -- * Type checked types
  , TcType(..)
  , TcRefType(..)
  , TcClassType(..)
  , TcTypeArgument
  ) where

import Data.Data

import Language.Java.Paragon.Annotation
import Language.Java.Paragon.Annotated
import Language.Java.Paragon.PolicyLang
import Language.Java.Paragon.Syntax

-- | For those elements in the AST where a type is not relevant the annotation
-- can be set to @Nothing@ using this function.
noTypeAnn :: Annotated ast => ast -> ast
noTypeAnn node = modifyAnn (\a -> a { annType = Nothing }) node

-- | Type checked types.
data TcType =
    TcPrimType PrimType  -- ^ Type checked primitive type.
  | TcRefType TcRefType  -- ^ Type checked reference type.
  | TcVoidType           -- ^ Type checked void type.
  deriving (Data, Typeable, Show, Eq, Ord)

-- | Type checked reference type.
data TcRefType =
    TcClassRefType TcClassType      -- ^ Reference is a class.
  | TcArrayType TcType ParagonMetaPolicy  -- ^ Type of the array and policy on elements.
  | TcTypeVar String                -- ^ Type is a variable (i.e. generic type)
  | TcNullType                      -- ^ Type of @null@.
  deriving (Data, Typeable, Show, Eq, Ord)

-- | A type checked class, with type checked class arguments.
data TcClassType = TcClassType Name [TcTypeArgument]
  deriving (Data, Typeable, Show, Eq, Ord)

-- | Unimplemented type checked type parameters.
data TcTypeArgument = TTP
  deriving (Data, Typeable, Show, Eq, Ord)
