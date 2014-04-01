module Language.Java.Paragon.Monad.TypeCheckM
  ( 
    -- * The @TypeCheck@ monad
    TypeCheckM
  , TypeCheckEnv(..)
  , TypeCheck
  , runTypeCheckM
  , getTypeCheckEnv
  , getTypeParamSubst
  , getThisType
  , getCurrentTypeMap
  , getGlobalTypeMap
  , setGlobalTypeMap
  , modifyGlobalTypeMap
  ) where

import Control.Applicative
import Control.Monad (ap, liftM)

import Language.Java.Paragon.Monad.Base
import Language.Java.Paragon.Monad.PiReader
import Language.Java.Paragon.Syntax

import Language.Java.Paragon.TypeChecker.Types

-- | Type checking environment contains the substitution of the type parameters
-- as well as the declaration currently under check.
data TypeCheckEnv = TypeCheckEnv
  { tcTypeParamSubst :: [(TypeParam, TcType)] 
  , tcThisDecl       :: TypeDecl
  , tcCurrentTypeMap :: TypeMap
  , tcGlobalTypeMap  :: TypeMap
  }

-- | A type check program returns an AST and the updated global type map. The
-- monad ensures that this global type map is propagated properly (as state).
newtype TypeCheckM a = TypeCheckM (TypeCheckEnv -> PiReader (a, TypeMap))

-- | Shorthand for type checking functions: they insert the type information in
-- the provided AST and return it.
type TypeCheck ast = ast -> TypeCheckM ast

runTypeCheckM :: [(TypeParam, TcType)] -> TypeDecl -> TypeCheckM a -> PiReader a
runTypeCheckM typeParamSubst thisDecl (TypeCheckM comp) = do
  let env = TypeCheckEnv { tcTypeParamSubst = typeParamSubst
                         , tcThisDecl       = thisDecl
                         , tcCurrentTypeMap = emptyTM
                         , tcGlobalTypeMap  = emptyTM }
  fst <$> comp env

-- | Monad instance; caries the environment around and updates the global type
-- map in the environment.
instance Monad TypeCheckM where
  return = liftPR . return
  TypeCheckM f >>= k = TypeCheckM $ \env -> do
                         (a, globalTM) <- f env
                         let TypeCheckM g = k a
                         g $ env { tcGlobalTypeMap = globalTM }
  fail = liftPR . fail

-- | Lifts PiReader computations. Returns the result of the PiReader computation
-- together with the current global type map.
instance MonadPR TypeCheckM where
  liftPR pr = TypeCheckM $ \env -> pr >>= \a -> return (a, tcGlobalTypeMap env)

-- | Mostly straightforward. The try function needs to push the either-result
-- inward.
instance MonadBase TypeCheckM where
  liftBase = liftPR . liftBase
  withErrCtxt erC (TypeCheckM f) = TypeCheckM $ \env -> withErrCtxt erC $ f env
  tryM (TypeCheckM f) = TypeCheckM $ \env -> do
                         errOrA <- tryM (f env)
                         case errOrA of
                           Right (a, globalTM) -> return (Right a, globalTM)
                           Left err -> return (Left err, tcGlobalTypeMap env)
  failE = liftPR . failE
  failEC x = liftPR . failEC x

instance MonadIO TypeCheckM where
  liftIO = liftPR . liftIO

instance Functor TypeCheckM where
  fmap = liftM

instance Applicative TypeCheckM where
  pure  = return
  (<*>) = ap

-- | Access environment.
getTypeCheckEnv :: TypeCheckM TypeCheckEnv
getTypeCheckEnv = TypeCheckM $ \s -> return s

-- | Access skolemising substitution.
getTypeParamSubst :: TypeCheckM [(TypeParam, TcType)] 
getTypeParamSubst = tcTypeParamSubst <$> getTypeCheckEnv

-- | Access the type declaration under current check.
getThisType :: TypeCheckM TypeDecl
getThisType = tcThisDecl <$> getTypeCheckEnv

-- | Access the current type map.
getCurrentTypeMap :: TypeCheckM TypeMap
getCurrentTypeMap = tcCurrentTypeMap <$> getTypeCheckEnv

-- | Access the global type map.
getGlobalTypeMap :: TypeCheckM TypeMap
getGlobalTypeMap = tcGlobalTypeMap <$> getTypeCheckEnv

-- | Update the global type map.
setGlobalTypeMap :: TypeMap -> TypeCheckM ()
setGlobalTypeMap tm = TypeCheckM $ \env -> return ((), tm)

-- | Modify the global type map.
modifyGlobalTypeMap :: (TypeMap -> TypeMap) -> TypeCheckM ()
modifyGlobalTypeMap f = TypeCheckM $ \env -> return ((), f (tcGlobalTypeMap env))
