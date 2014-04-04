{-# LANGUAGE DeriveDataTypeable #-}
module Language.Java.Paragon.TypeChecker.TcTypes 
  (
    TcType(..)
  , TcRefType(..)
  , TcClassType(..)
  , TcTypeParam
  ) where

import Data.Data

import Language.Java.Paragon.PolicyLang
import Language.Java.Paragon.Syntax

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

-- | A type checked class, with type checked class parameters.
data TcClassType = TcClassType Name [TcTypeParam]
  deriving (Data, Typeable, Show, Eq, Ord)

-- | Unimplemented type checked type parameters.
data TcTypeParam = TTP
  deriving (Data, Typeable, Show, Eq, Ord)
