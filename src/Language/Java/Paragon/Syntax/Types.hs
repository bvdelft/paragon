{-# LANGUAGE DeriveFunctor #-}

-- | Paragon Abstract Syntax Tree. Types.
module Language.Java.Paragon.Syntax.Types where

import Language.Java.Paragon.Syntax.Names

-- | Top-level data type for Paragon types.
data Type a =
    -- | Primitive type.
    PrimType { typeAnn      :: a           -- ^ Annotation.
             , typePrimType :: PrimType a  -- ^ Primitive type.
             }
    -- | Reference type.
  | RefType { typeAnn     :: a
            , typeRefType :: RefType a  -- ^ Reference type.
            }
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
    ClassRefType { refTypeAnn       :: a            -- ^ Annotation.
                 , refTypeClassType :: ClassType a  -- ^ Class type.
                 }
  -- TODO: ArrayType
  deriving (Show, Eq, Functor)

-- | Class or interface type.
data ClassType a = ClassType a (Name a) [TypeArgument a]
  deriving (Show, Eq, Functor)

-- | Representation of type arguments of generic types.
data TypeArgument a = TA
  deriving (Show, Eq, Functor)

-- | Representation of possible return types.
data ReturnType a =
    -- | void.
    VoidType { retTypeAnn :: a }
    -- | Lock type.
  | LockType { retTypeAnn :: a }
    -- | Other types.
  | Type { retTypeAnn :: a
         , retType    :: Type a
         }
  deriving (Show, Eq, Functor)

