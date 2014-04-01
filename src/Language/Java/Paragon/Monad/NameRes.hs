module Language.Java.Paragon.Monad.NameRes
  (
    NameRes(..)
  , NameResEnv(..)
  , Resolve
  , getNameResEnv
  , getExpansion
  , getCurrentName
  , withExpansion
  , extendExpansion
  ) where

import Control.Applicative
import Control.Monad (ap, liftM)

import Language.Java.Paragon.Monad.Base
import Language.Java.Paragon.Monad.PiReader
import Language.Java.Paragon.NameResolution.Expansion
import Language.Java.Paragon.Syntax

data NameResEnv = NameResEnv
  { nrCurrentName :: Name
  , nrExpansion   :: Expansion
  }

newtype NameRes a = 
  NameRes { runNameRes :: NameResEnv -> PiReader a }
  
type Resolve ast = ast -> NameRes ast

instance Monad NameRes where
  return          = liftPR . return
  NameRes f >>= k = NameRes $ \env -> do
                         a <- f env
                         let NameRes g = k a
                          in g env
  fail            = liftPR . fail

instance MonadPR NameRes where
  liftPR pr = NameRes $ \_ -> pr

instance MonadBase NameRes where
  liftBase                    = liftPR . liftBase
  withErrCtxt erC (NameRes f) = NameRes $ (withErrCtxt erC) . f
  tryM (NameRes f)            = NameRes $ tryM . f
  failE                       = liftPR . failE
  failEC x                    = liftPR . failEC x

instance MonadIO NameRes where
  liftIO = liftPR . liftIO

instance Functor NameRes where
  fmap = liftM

instance Applicative NameRes where
  pure  = return
  (<*>) = ap

-- | Access environment 
getNameResEnv :: NameRes NameResEnv
getNameResEnv = NameRes $ \s -> return s

-- | Access expansion map
getExpansion :: NameRes Expansion
getExpansion = nrExpansion <$> getNameResEnv

-- | Access name of currently handled syntactical unit
getCurrentName :: NameRes Name
getCurrentName = nrCurrentName <$> getNameResEnv

-- | Set expansion map for given NameRes computation
withExpansion :: Expansion -> NameRes a -> NameRes a
withExpansion e (NameRes f) = NameRes $ \env -> f (env {nrExpansion = e})

-- | Extend expansion map of given computation by given expansion map
extendExpansion :: Expansion -> NameRes a -> NameRes a
extendExpansion e1 nra = do
  e2 <- getExpansion
  withExpansion (expansionUnion [e1,e2]) nra
