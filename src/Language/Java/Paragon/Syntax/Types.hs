-- | Paragon Abstract Syntax Tree. Types.
module Language.Java.Paragon.Syntax.Types
  (
    module Language.Java.Paragon.Syntax.Types
  , module Language.Java.Paragon.Syntax.Names
  ) where

import Language.Java.Paragon.Syntax.Names

import Language.Java.Paragon.Annotation
import Language.Java.Paragon.Annotated

-- | Top-level data type for Paragon types.
data Type =
    -- | Primitive type.
    PrimType PrimType
    -- | Reference type.
  | RefType RefType
  deriving (Show, Eq)

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
  deriving (Show, Eq)
  
-- | Reference type.
data RefType =
    -- | Class type.
    ClassRefType ClassType
  -- TODO: ArrayType
  deriving (Show, Eq)

-- | Class or interface type.
data ClassType =
    -- | Class type.
    ClassType { ctAnn      :: Annotation
              , ctName     :: Name
              , ctTypeArgs :: [TypeArgument]
              }
  deriving (Show, Eq)

-- | Representation of type arguments of generic types.
data TypeArgument = TA
  deriving (Show, Eq)

-- | Representation of possible return types.
data ReturnType =
    -- | void.
    VoidType Annotation
    -- | Lock type.
  | LockType Annotation
    -- | Other types.
  | Type Type
  deriving (Show, Eq)

-- | If given argument represents void or lock type - returns nothing.
-- Otherwise - return Just type it represents.
returnTypeToType :: ReturnType -> Maybe Type
returnTypeToType (Type t) = Just t
returnTypeToType        _ = Nothing

instance Annotated Type where
  ann (PrimType x) = ann x
  ann (RefType  x) = ann x

instance Annotated PrimType where
  ann (BooleanT x) = x
  ann (ByteT    x) = x
  ann (ShortT   x) = x
  ann (IntT     x) = x
  ann (LongT    x) = x
  ann (CharT    x) = x
  ann (FloatT   x) = x
  ann (DoubleT  x) = x
  ann (PolicyT  x) = x
  
instance Annotated RefType where
  ann (ClassRefType x) = ann x

instance Annotated ClassType where
  ann = ctAnn

instance Annotated ReturnType where
  ann (VoidType x) = x
  ann (LockType x) = x
  ann (Type t)     = ann t
