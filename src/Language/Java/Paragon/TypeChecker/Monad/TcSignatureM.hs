-- | This module provides a monad for typechecking the signatures of Paragon
-- packages, classes, methods and fields.
module Language.Java.Paragon.TypeChecker.Monad.TcSignatureM
  ( 
    -- * The @TcSignature@ monad
    TcSignatureM
  , TcSignatureEnv(..)
  , TcSignatureState
  , TcSignature
  , runTcSignatureM
  , getTcSignatureEnv
  , getThisType
  , getTypeMap
  , getPkgMap
  , setPkgMap
  , modifyPkgMap
  ) where

import Control.Applicative
import Control.Monad (ap, liftM)

import Language.Java.Paragon.Monad.Base
import Language.Java.Paragon.Monad.PiReader
import Language.Java.Paragon.Syntax

import Language.Java.Paragon.TypeChecker.TypeMap
import Language.Java.Paragon.TypeChecker.PkgMap

-- | Signature type checking environment.
data TcSignatureEnv = TcSignatureEnv
  { tcsThisDecl :: TypeDecl  -- ^ Declaration for which we build the @TypeMap@.
  , tcsTypeMap  :: TypeMap   -- ^ Signatures for current declaration.
  }

type TcSignatureState = PkgMap

-- | A type check program returns an AST and the updated package map. The
-- monad ensures that this global type map is propagated properly (as state).
newtype TcSignatureM a = TcSignatureM
  (TcSignatureEnv -> TcSignatureState -> PiReader (a, TcSignatureState))

-- | Shorthand for type checking signatures: they insert the type information in
-- the provided AST and return it.
type TcSignature ast = ast -> TcSignatureM ast

-- | Run a @TcSignatureM@ computation.
runTcSignatureM :: TypeDecl        -- ^ The declaration that is to be checked.
                -> TcSignatureM a  -- ^ The computation checking signatures.
                -> PiReader a      -- ^ Result of signature computation.
runTcSignatureM thisDecl (TcSignatureM comp) = do
  let env = TcSignatureEnv { tcsThisDecl = thisDecl
                           , tcsTypeMap  = emptyTypeMap }
  fst <$> comp env emptyPkgMap

-- | Monad instance; caries the environment around and updates the global
-- package map.
instance Monad TcSignatureM where
  return = liftPR . return
  TcSignatureM f >>= k = TcSignatureM $ \env st -> do
                         (a, globalPM) <- f env st
                         let TcSignatureM g = k a
                         g env globalPM
  fail = liftPR . fail

-- | Lifts PiReader computations. Returns the result of the PiReader computation
-- together with the current global package map.
instance MonadPR TcSignatureM where
  liftPR pr = TcSignatureM $ \_ globalPM -> pr >>= \a -> return (a, globalPM)

-- | Mostly straightforward. The try function needs to push the either-result
-- inward.
instance MonadBase TcSignatureM where
  liftBase = liftPR . liftBase
  withErrCtxt erC (TcSignatureM f) = 
    TcSignatureM $ \env globalTM -> withErrCtxt erC $ f env globalTM
  tryM (TcSignatureM f) = TcSignatureM $ \env globalTM -> do
                            errOrA <- tryM (f env globalTM)
                            case errOrA of
                              Right (a, globalTM') -> return (Right a, globalTM')
                              Left err -> return (Left err, globalTM)
  failE = liftPR . failE
  failEC x = liftPR . failEC x

instance MonadIO TcSignatureM where
  liftIO = liftPR . liftIO

instance Functor TcSignatureM where
  fmap = liftM

instance Applicative TcSignatureM where
  pure  = return
  (<*>) = ap

-- | Access environment.
getTcSignatureEnv :: TcSignatureM TcSignatureEnv
getTcSignatureEnv = TcSignatureM $ \e s -> return (e, s)

-- | Access the type declaration under current check.
getThisType :: TcSignatureM TypeDecl
getThisType = tcsThisDecl <$> getTcSignatureEnv

-- | Access the current type map.
getTypeMap :: TcSignatureM TypeMap
getTypeMap = tcsTypeMap <$> getTcSignatureEnv

-- | Access the package map.
getPkgMap :: TcSignatureM PkgMap
getPkgMap = TcSignatureM $ \_ s -> return (s, s)

-- | Update the package map.
setPkgMap :: PkgMap -> TcSignatureM ()
setPkgMap pm = TcSignatureM $ \_ _ -> return ((), pm)

-- | Modify the package map.
modifyPkgMap :: (PkgMap -> PkgMap) -> TcSignatureM ()
modifyPkgMap f = TcSignatureM $ \_ pm -> return ((), f pm)
