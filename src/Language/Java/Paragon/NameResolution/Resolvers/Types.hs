module Language.Java.Paragon.NameResolution.Resolvers.Types
  (
    -- * Resolvers
    rnType
  , rnRefType
  , rnReturnType
  ) where

import Language.Java.Paragon.Monad.NameRes
import Language.Java.Paragon.Syntax.Types

import Language.Java.Paragon.NameResolution.Resolvers.Names

-- | Resolve a top-level data type.
rnType :: Resolve Type
rnType (PrimType primType) = return $ PrimType primType
rnType (RefType  refType) = do
  r <- rnRefType refType
  return $ RefType r

-- | Resolve a reference type.
rnRefType :: Resolve RefType
rnRefType (ClassRefType classType) = do
  c <- rnClassType classType
  return $ ClassRefType c

-- | Resolve a class or interface type.
rnClassType :: Resolve ClassType
rnClassType classType = do
  -- TODO: type arguments
  name <- rnName (ctName classType)
  return $ classType { ctName = name }

-- | Resolve a return type.
rnReturnType :: Resolve ReturnType
rnReturnType voidType@(VoidType {}) = return voidType
rnReturnType lockType@(LockType {}) = return lockType
rnReturnType (Type ty) = do res <- rnType ty
                            return $ Type res
