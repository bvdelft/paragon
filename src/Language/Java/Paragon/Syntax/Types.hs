{-# LANGUAGE DeriveDataTypeable #-}
-- | Paragon Abstract Syntax Tree. Types.
module Language.Java.Paragon.Syntax.Types
  (
    module Language.Java.Paragon.Syntax.Types
  , module Language.Java.Paragon.Syntax.Names
  ) where

import Data.Data

import Language.Java.Paragon.Syntax.Names

import Language.Java.Paragon.Annotation
import Language.Java.Paragon.Annotated

-- | Top-level data type for Paragon types.
data Type =
    -- | Primitive type.
    PrimType PrimType
    -- | Reference type.
  | RefType RefType
  deriving (Data, Typeable, Ord, Show, Eq)

-- | Primitive types.
data PrimType =
    BooleanT Annotation
  | ByteT    Annotation
  | ShortT   Annotation
  | IntT     Annotation
  | LongT    Annotation
  | CharT    Annotation
  | FloatT   Annotation
  | DoubleT  Annotation
  -- Paragon specific
  | PolicyT  Annotation
  deriving (Data, Typeable, Show, Eq, Ord)
  
-- | Reference type.
data RefType =
    -- | Class type.
    ClassRefType ClassType
  -- TODO: ArrayType
  deriving (Data, Typeable, Ord, Show, Eq)

-- | Class or interface type.
data ClassType =
    -- | Class type.
    ClassType { ctAnn      :: Annotation
              , ctName     :: Name
              , ctTypeArgs :: [TypeArgument]
              }
  deriving (Data, Typeable, Ord, Show, Eq)

-- | Representation of type arguments of generic types.
data TypeArgument = TA
  deriving (Data, Typeable, Ord, Show, Eq)

-- | Representation of possible return types.
data ReturnType =
    -- | void.
    VoidType Annotation
    -- | Lock type.
  | LockType Annotation
    -- | Other types.
  | Type Type
  deriving (Data, Typeable, Ord, Show, Eq)

-- | If given argument represents void or lock type - returns nothing.
-- Otherwise - return Just type it represents.
returnTypeToType :: ReturnType -> Maybe Type
returnTypeToType (Type t) = Just t
returnTypeToType        _ = Nothing

instance Annotated Type where
  getAnn (PrimType x) = getAnn x
  getAnn (RefType  x) = getAnn x
  setAnn a (PrimType x) = PrimType $ setAnn a x
  setAnn a (RefType  x) = RefType  $ setAnn a x

instance Annotated PrimType where
  getAnn (BooleanT x) = x
  getAnn (ByteT    x) = x
  getAnn (ShortT   x) = x
  getAnn (IntT     x) = x
  getAnn (LongT    x) = x
  getAnn (CharT    x) = x
  getAnn (FloatT   x) = x
  getAnn (DoubleT  x) = x
  getAnn (PolicyT  x) = x
  setAnn a (BooleanT _) = BooleanT a
  setAnn a (ByteT    _) = ByteT    a
  setAnn a (ShortT   _) = ShortT   a
  setAnn a (IntT     _) = IntT     a
  setAnn a (LongT    _) = LongT    a
  setAnn a (CharT    _) = CharT    a
  setAnn a (FloatT   _) = FloatT   a
  setAnn a (DoubleT  _) = DoubleT  a
  setAnn a (PolicyT  _) = PolicyT  a
  
instance Annotated RefType where
  getAnn (ClassRefType x) = getAnn x
  setAnn a (ClassRefType x) = ClassRefType $ setAnn a x

instance Annotated ClassType where
  getAnn = ctAnn
  setAnn a x = x { ctAnn = a }

instance Annotated ReturnType where
  getAnn (VoidType x) = x
  getAnn (LockType x) = x
  getAnn (Type t)     = getAnn t
  setAnn a (VoidType _) = VoidType a
  setAnn a (LockType _) = LockType a
  setAnn a (Type t)     = Type $ setAnn a t
