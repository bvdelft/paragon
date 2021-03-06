module Language.Java.Paragon.TypeChecker.Types 
  (
    TcType(..)
  ) where

import Language.Java.Paragon.Syntax

-- | Type checked types.
data TcType =
    TcPrimType PrimType  -- ^ Type checked primitive type.
  | TcRefType TcRefType  -- ^ Type checked reference type.
  | TcVoidType           -- ^ Type checked void type.
  deriving (Show, Eq, Ord)

-- | Type checked reference type.
data TcRefType =
    TcClassRefType TcClassType      -- ^ Reference is a class.
  | TcArrayType TcType ActorPolicy  -- ^ Type of the array and policy on elements.
  | TcTypeVar String                -- ^ Type is a variable (i.e. generic type)
  | TcNullType                      -- ^ Type of @null@.
  deriving (Show, Eq, Ord)

-- | A type checked class, with type checked class parameters.
data TcClassType = TcClassType Name [TcTypeParam]
  deriving (Show, Eq, Ord)

-- | Unimplemented type checked type parameters.
data TcTypeParam = TTP
  deriving (Show, Eq, Ord)
