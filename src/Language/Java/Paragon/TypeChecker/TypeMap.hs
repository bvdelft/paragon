{-# LANGUAGE DeriveDataTypeable #-}
-- | This module provides the @TypeMap@ data structure, as well as several 
-- data structures for the type signatures of the various elements stored in a
-- @TypeMap@.
module Language.Java.Paragon.TypeChecker.TypeMap
  (
    module Language.Java.Paragon.TypeChecker.TypeMap
  ) where

import Data.Data
import Data.Map (Map, empty)

import Language.Java.Paragon.PolicyLang
import Language.Java.Paragon.Syntax

import Language.Java.Paragon.TypeChecker.Types

-- | The @TypeMap@ data structure.
-- For methods and constructors, which might share the same name, we store an
-- additional mapping, from the number and type of arguments to the actual
-- signature.
data TypeMap = TypeMap
  { tmFields       :: Map String FieldSignature
  , tmMethods      :: Map String MethodMap
  , tmConstructors :: ConstructorMap
  , tmLocks        :: Map String ()
  , tmPolicies     :: Map String ParagonVarPolicy
  , tmActors       :: Map String ()
  , tmTypeMethods  :: Map String ()
  , tmTypes        :: Map String TypeSignature  -- ^ Inner types.
  } deriving (Show, Data, Typeable)

emptyTypeMap :: TypeMap
emptyTypeMap = TypeMap
  { tmFields       = empty
  , tmMethods      = empty
  , tmConstructors = empty
  , tmLocks        = empty
  , tmPolicies     = empty
  , tmActors       = empty
  , tmTypeMethods  = empty
  , tmTypes        = empty
  }

data FieldSignature = FieldSignature
  { fsType     :: TcType       -- ^ The type checked type of this field.
  , fsPolicy   :: ParagonVarPolicy
  , fsIsStatic :: Bool         -- ^ Whether this field is declared @static@.
  , fsIsFinal  :: Bool         -- ^ Whether this field is declared @final@.
  , fsNotNull  :: Bool         -- ^ Whether this field is declared @notnull@.
  } deriving (Show, Data, Typeable)

-- | For method disambiguation, this mapping provides the method signature for
-- each method by number of type parameters, real arguments and whether it has
-- varargs.
type MethodMap = Map ([TypeParam], [TcType], Bool) MethodSig

data MethodSig = MethodSig
  { msReturnType      :: TcType       -- ^ Return type.
  , msModifiers       :: [Modifier]   -- ^ Method modifiers.
  , msReadEffect      :: ParagonVarPolicy  -- ^ Read effect policy.
  , msWriteEffect     :: ParagonVarPolicy  -- ^ Write effect policy.
  , msExpectedLocks   :: [()]     -- ^ Locks expected to be open.
  , msParameters      :: Map String ParameterSig  -- ^ Method's parameters.
  , msLockDelta       :: ()  -- ^ Which locks are opened and closed.
  , msExceptions      :: Map TcType ExceptionSignature  -- ^ Signature per exception thrown.
  , msIsNative        :: Bool         -- ^ Whether this method is native (to Paragon)
  } deriving (Show, Data, Typeable)

data ParameterSig = ParameterSig
  { psBound   :: ParagonVarPolicy  -- ^ Policy on this parameter.
  , psNotNull :: Bool         -- ^ Whether this parameter is labelled as not null.
  } deriving (Show, Data, Typeable)

data ExceptionSignature = ExceptionSignature
  { exsReadEffect   :: ParagonVarPolicy  -- ^ Read effect policy.
  , exsWriteEffect  :: ParagonVarPolicy  -- ^ Write effect policy.
  , exsLockDelta    :: ()       -- ^ Locks opened and closed when exception is thrown.
  } deriving (Show, Data, Typeable)

-- | For constructor disambiguation, this mapping provides the constructor
-- signature for each constructor by number of type parameters, real arguments
-- and whether the last argument is a @varargs@.
type ConstructorMap = Map ([TypeParam], [TcType], Bool) ConstructorSignature

data ConstructorSignature = ConstructorSignature
  { csModifiers       :: [Modifier]     -- ^ Constructor modifiers.
  , csWriteEffect     :: ParagonVarPolicy    -- ^ Write effect policy.
  , csExpectedLocks   :: [()]       -- ^ Locks expected to be open.
  , csParameters      :: Map String ParagonVarPolicy  -- ^ Constructor's parameters.
  , csLockDelta       :: ()    -- ^ Which locks are opened and closed.
  , csExceptions      :: [(TcType, ExceptionSignature)]  -- ^ Signature per exception thrown.
  , csIsNative        :: Bool           -- ^ Whether this constructor is native (to Paragon).
  } deriving (Show, Data, Typeable)

data TypeSignature = TypeSignature
  { tsType         :: TcRefType      -- ^ This reference type.
  , tsIsClass      :: Bool           -- ^ Whether this is a class or an interface.
  , tsIsFinal      :: Bool           -- ^ Whether this type is declared final.
  , tsSuperClasses :: [TcClassType]  -- ^ Super class (possibly multiple for interfaces).
  , tsInterfaces   :: [TcClassType]  -- ^ Interfaces implemented by this type.
  , tsMembers      :: TypeMap        -- ^ Members of this type.
  } deriving (Show, Data, Typeable)
