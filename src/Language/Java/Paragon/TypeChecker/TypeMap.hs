-- | This module provides the @TypeMap@ data structure, as well as several 
-- data structures for the type signatures of the various elements stored in a
-- @TypeMap@.
module Language.Java.Paragon.TypeChecker.TypeMap
  (
    TypeMap
  ) where

import Language.Java.Paragon.TypeChecker.Types

-- | The @TypeMap@ data structure.
-- For methods and constructors, which might share the same name, we store an
-- additional mapping, from the number and type of arguments to the actual
-- signature.
-- A package is mapped to a @TypeMap@ for that package (??)
data TypeMap = TypeMap
  { tmFields       :: Map String FieldSignature
  , tmMethods      :: Map String MethodMap
  , tmConstructors :: ConstructorMap
  , tmLocks        :: Map String ()
  , tmPolicies     :: Map String PrgPolicy      -- TODO: WHAT IS THIS?
  , tmActors       :: Map String ()
  , tmTypeMethods  :: Map String ()
  , tmTypes        :: Map String TypeSignature
  , tmPackages     :: Map String TypeMap
  } deriving (Show)

data FieldSignature = FieldSignature
  { fsType     :: TcType       -- ^ The type checked type of this field.
  , fsPolicy   :: ActorPolicy  -- TODO: WHAT IS THIS?
  , fsIsParam  :: Bool         -- TODO: WHAT IS THIS?
  , fsIsStatic :: Bool         -- ^ Whether this field is declared @static@.
  , fsIsFinal  :: Bool         -- ^ Whether this field is declared @final@.
  , fsNotNull  :: Bool         -- ^ Whether this field is declared @notnull@.
  } deriving (Show)

-- | For method disambiguation, this mapping provides the method signature for
-- each method by number of type parameters, real arguments and ??? (TODO).
type MethodMap = Map ([TypeParam], [TcType], Bool) MethodSig

data MethodSig = MethodSig
  { msReturnType      :: TcType         -- ^ Return type.
  , msModifiers       :: [Modifier]     -- ^ Method modifiers.
  , msReadEffect      :: ActorPolicy    -- ^ Read effect policy.
  , msWriteEffect     :: ActorPolicy    -- ^ Write effect policy.
  , msExpectedLocks   :: [TcLock]       -- ^ Locks expected to be open.
  , msParameters      :: [String]       -- ^ Method's parameters.
  , msParameterBounds :: [ActorPolicy]  -- ^ Policy read-effect bounds on parameters. TODO: why not paired with params?
  , msLockDelta       :: TcLockDelta    -- ^ Which locks are opened and closed.
  , msExceptions      :: [(TcType, ExceptionSignature)]  -- ^ Signature per exception thrown.
  , msNNPars          :: [String]  -- TODO: WHAT IS THIS?
  , msIsNative        :: Bool           -- ^ Whether this method is native (to Paragon)
  }

data ExceptionSignature = ExceptionSignature
  { exsReadEffect   :: ActorPolicy  -- ^ Read effect policy.
  , exsWriteEffect  :: ActorPolicy  -- ^ Write effect policy.
  , exsLockDelta    :: TcLockDelta  -- ^ Locks opened and closed when exception is thrown.
  }

-- | For constructor disambiguation, this mapping provides the constructor
-- signature for each constructor by number of type parameters, real arguments
-- and ??? (TODO).
type ConstructorMap = Map ([TypeParam], [TcType], Bool) ConstructorSig

data ConstructorSignature = ConstructorSignature
  { csModifiers       :: [Modifier]     -- ^ Constructor modifiers.
  , csWriteEffect     :: ActorPolicy    -- ^ Write effect policy.
  , csExpectedLocks   :: [TcLock]       -- ^ Locks expected to be open.
  , csParameters      :: [String]       -- ^ Constructor's parameters.
  , csParameterBounds :: [ActorPolicy]  -- ^ Policy read-effect bounds on parameters.
  , csLockDelta       :: TcLockDelta    -- ^ Which locks are opened and closed.
  , csExceptions      :: [(TcType, ExceptionSignature)]  -- ^ Signature per exception thrown.
  , csNNPars          :: [String]       -- ^ TODO ??
  , csIsNative        :: Bool           -- ^ Whether this constructor is native (to Paragon).
  }

data TypeSignature = TypeSignature
  { tsType         :: TcRefType      -- ^ This reference type.
  , tsIsClass      :: Bool           -- ^ Whether this is a class (or an interface?) TODO
  , tsIsFinal      :: Bool           -- ^ Whether this type is declared final.
  , tsSuperClasses :: [TcClassType]  -- ^ Super classes, nearest super class first (TODO: check).
  , tsInterfaces   :: [TcClassType]  -- ^ Interfaces implemented by this type.
  , tsMembers      :: TypeMap        -- ^ Members of this type.
  }
