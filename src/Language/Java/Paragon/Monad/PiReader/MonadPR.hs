-- | Defines the @MonadPR@ monad; adding the PiPath.
module Language.Java.Paragon.Monad.PiReader.MonadPR
  ( 
    module Language.Java.Paragon.Monad.Base
  , PiPath
  , PiReader(..)
  , liftToBaseM
  , MonadPR(..)
  , getPiPath
  ) where

import Control.Applicative
import Control.Monad (liftM)

import Language.Java.Paragon.Monad.Base

-- | The PiPath is, like the Java classpath, a list of several locations where
-- .pi files can be found.
type PiPath = [FilePath]

-- | Monad that adding the PiPath environment to the base monad
newtype PiReader a = PiReader ( PiPath -> BaseM a )

instance Applicative (PiReader) where
  pure = return
  PiReader f <*> PiReader x = PiReader $ \ppath -> do 
                                a <- x ppath
                                g <- f ppath
                                return $ g a

-- | Standard reader monad behavior.
instance Monad (PiReader) where
  return x = PiReader $ \_ -> return x
  PiReader f >>= k = PiReader $ \pp -> do
                          a <- f pp
                          let PiReader g = k a
                           in g pp
  fail = liftBase . fail

-- | Transform a @PiReader@ into a @BaseM@ computation by providing a pi-path.
liftToBaseM :: PiPath -> PiReader a -> BaseM a
liftToBaseM pp (PiReader f) = f pp

instance Functor (PiReader) where
  fmap = liftM

instance MonadBase (PiReader) where
  liftBase ba = PiReader $ const ba
  withErrCtxt ecf (PiReader f) = PiReader $ withErrCtxt ecf . f
  tryM (PiReader f) = PiReader $ tryM . f
  failE = liftBase . failE
  failEC x = liftBase . failEC x

instance MonadIO (PiReader) where
  liftIO = liftBase . liftIO

class MonadBase m => MonadPR m where
  -- | Lift PiReader computations to the member of the @MonadPR@ class.
  liftPR :: PiReader a -> m a

instance MonadPR PiReader where
  liftPR = id

-- | Read the PiPath from the environment.
getPiPathPR :: PiReader PiPath
getPiPathPR = PiReader return

-- | Read the PiPath from the inner environment.
getPiPath :: MonadPR m => m PiPath
getPiPath = liftPR getPiPathPR
