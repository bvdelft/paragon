{-# LANGUAGE TemplateHaskell
           , DeriveFunctor
 #-}

-- | Paragon Abstract Syntax Tree. Types.
module Language.Java.Paragon.Syntax.Types
  (
    module Language.Java.Paragon.Syntax.Types
  , module Language.Java.Paragon.Syntax.Names
  ) where

import Language.Java.Paragon.Syntax.Names

import Language.Java.Paragon.Annotated

-- | Top-level data type for Paragon types.
data Type a =
    -- | Primitive type.
    PrimType (PrimType a)
    -- | Reference type.
  | RefType (RefType a)
  deriving (Show, Eq, Functor)

-- | Primitive types.
data PrimType a =
    BooleanT a
  | ByteT    a
  | ShortT   a
  | IntT     a
  | LongT    a
  | CharT    a
  | FloatT   a
  | DoubleT  a
  -- Paragon specific
  | PolicyT  a
  deriving (Show, Eq, Functor)

-- | Reference type.
data RefType a =
    -- | Class type.
    ClassRefType (ClassType a)
  -- TODO: ArrayType
  deriving (Show, Eq, Functor)

-- | Class or interface type.
data ClassType a =
    -- | Class type.
    ClassType { ctAnn      :: a 
              , ctName     :: Name a
              , ctTypeArgs :: [TypeArgument a]
              }
  deriving (Show, Eq, Functor)

-- | Representation of type arguments of generic types.
data TypeArgument a = TA
  deriving (Show, Eq, Functor)

-- | Representation of possible return types.
data ReturnType a =
    -- | void.
    VoidType a
    -- | Lock type.
  | LockType a
    -- | Other types.
  | Type (Type a)
  deriving (Show, Eq, Functor)

$(deriveAnnotatedMany
  [ ''Type
  , ''PrimType
  , ''RefType
  , ''ClassType
  , ''ReturnType
  ])

