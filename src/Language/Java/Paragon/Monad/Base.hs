{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
-- | This module contains the implementation of the BaseM monad, the monad
-- that is at the bottom of our monad stack and adds error-handling and
-- unique number generation on top of IO.
module Language.Java.Paragon.Monad.Base 
  (
    -- * The @BaseM@ monad 
    BaseM
  , runBaseM
  , raiseErrors
    -- * The @MonadIO@ class
  , MonadIO(..)
    -- * The @MonadBase@ class
  , MonadBase(..) 
  , liftEitherMB
  , tryCatch
  , getFlags
  , getFreshInt
  , getErrCtxt
  , check
  , checkM
  ) where

import Control.Applicative
import Control.Monad
import Data.Monoid

import Language.Java.Paragon.Error
import Language.Java.Paragon.Flags

-- | State of the base monad, as a record for easier extensibility.
data BaseState = BaseState
  { -- | For generating fresh integers.
    bNextUniq     :: Int
  }

-- | Reader environment for the base monad.
data BaseEnv = BaseEnv
  { -- | The flags given to the compilation.
    bFlags        :: [Flag]
    -- | Current context (class, method) to identify where errors are raised.
  , bErrorContext :: [ErrorContext]
  }

-- | Writer of the base monad, accumulates the errors raised by the compiler.
data BaseWrite = BaseWrite
  { -- | Accumulates the errors.
    bErrors       :: [Error]
  }

-- | The writer of the monad is a monoid, mempty being the initial write result,
-- mappend joining two write results.
instance Monoid BaseWrite where
  mempty      = BaseWrite { bErrors = [] }
  mappend a b = a { bErrors = (bErrors a) ++ (bErrors b) }

-- | The base monad adds error handling and unique number generation to IO.
-- The error-handling is somewhat involved because we assume the existence of
-- both /fatal/ and /non-fatal/ errors: Errors are accumulated according to the
-- Writer pattern, and the Maybe monad is used to short-circuit computation in
-- case of fatal errors. If a non-fatal error occurs, the compuation continues
-- with a Just value, but the list of errors is extended.
newtype BaseM a =
  BaseM (BaseEnv -> BaseState -> IO (Maybe a, BaseState, BaseWrite))

instance Monad BaseM where
  return x      = BaseM $ \_ st -> return (Just x, st, mempty)
  BaseM f >>= k = 
      BaseM $ \env st ->
          do (maybeA, stA, wrA) <- f env st
             case maybeA of
               Nothing -> return (Nothing, stA, wrA)
               Just a  -> do let BaseM g = k a
                             (maybeB, stB, wrB) <- g env stA
                             return (maybeB, stB, mappend wrA wrB)

  -- Provided for the sake of completeness;
  -- failE and failEC should be used instead
  fail err = failE $ undefinedError err

instance Applicative BaseM where
  pure = return
  (<*>) = ap

instance Functor BaseM where
  fmap = liftM

class Monad m => MonadIO m where
  liftIO :: IO a -> m a

instance MonadIO IO where
  liftIO = id

instance MonadIO BaseM where
  liftIO ioa = BaseM $ \_ st -> do x <- ioa
                                   return (Just x, st, mempty)

class MonadIO m => MonadBase m where
  -- | Lift BaseM computations to member of the MonadBase class
  liftBase :: BaseM a -> m a
  -- | Run computation with modified error context
  withErrCtxt :: ErrorContext -> m a -> m a
  -- | Try computation, returning the result in case of success or the errors
  tryM :: m a -> m (Either [Error] a)
  -- | Fatal error: abort computation (like fail, but on Error, not String)
  failE :: ContextualError -> m a       
  -- | Non-fatal error: continue computation from provided default value
  failEC :: a -> ContextualError -> m a

instance MonadBase BaseM where
  liftBase = id

  -- Run the subcomputation in an extended error context.
  withErrCtxt ectxt (BaseM f) = BaseM $ \env st ->
    do let newEnv = env {bErrorContext = (bErrorContext env) ++ [ectxt]}
       f newEnv st

  -- Construct Either value based on error state
  -- Note that tryM does not distinguish between fatal and non-fatal errors:
  -- If an error occured, we return the error rather than adding it to the
  -- writer part of the monad.
  tryM (BaseM f) = BaseM $ \env st ->
    do (maybeA, stA, wrA) <- f env st
       case bErrors wrA of
         [] -> return (fmap Right $ maybeA, stA, wrA)  
         e  -> return (Just . Left $ e, stA, wrA {bErrors = mempty} )
        
  -- Fatal error => Log error, but Nothing from here
  failE  err     = BaseM $ \env st ->
    return (Nothing, st, mempty { bErrors = [err $ bErrorContext env]})
       
  -- Non-fatal error => Log error, but continue with default value
  failEC def err = BaseM $ \env st ->
    return (Just def, st, mempty { bErrors = [err $ bErrorContext env]})

-- | Running a BaseM computation, which will result in either a
-- non-empty list of errors or the result of successfully running
-- the monadic computation
runBaseM :: [Flag] -> BaseM a -> IO (Either [Error] a)
runBaseM flags (BaseM f) = do
  (maybeA, _, wr) <- f (BaseEnv   { bErrorContext = []
                                  , bFlags        = flags
                                  } )
                       (BaseState { bNextUniq     = 0
                                  } )
  case bErrors wr of
    [] -> let (Just a) = maybeA
          in return $ Right a
    e  -> return . Left $ e

-- | Run a computation and afterwards fail if any errors, including non-fatal
-- ones, have been raised. This function can be used to e.g. wrap a phase, where
-- the phase itself might continue on non-fatal errors, but the rest of the
-- computation may not.
raiseErrors :: BaseM a -> BaseM a
raiseErrors (BaseM f) = BaseM $ \env st ->
  do (maybeA, stA, wrA) <- f env st
     case bErrors wrA of
       [] -> return (maybeA, stA, wrA)
       _  -> return (Nothing, stA, wrA)

-- | Get a fresh, unused number from the unique number generator
getFreshInt :: MonadBase m => m Int
getFreshInt = liftBase $ BaseM $ \_ st ->
  let n = bNextUniq st
  in  return (Just n, st {bNextUniq = n + 1}, mempty)
  

-- | Get the flags passed to the compiler
getFlags :: MonadBase m => m [Flag]
getFlags = liftBase $ BaseM $ \env st ->
  return (Just $ bFlags env, st, mempty)

-- | Access the ErrCtxt environment
getErrCtxt :: MonadBase m => m [ErrorContext]
getErrCtxt = liftBase $ BaseM $ \env st ->
  return (Just $ bErrorContext env, st, mempty)

-- | Try first argument. If and only if it fails, execute catch computation 
-- given as second argument.
tryCatch :: MonadBase m => m a -> ([Error] -> m a) -> m a
tryCatch tr ctch = do esa <- tryM tr
                      case esa of
                        Right a -> return a
                        Left err -> ctch err

-- | Lift Either value into monad by mapping Left to fail and Right to return
liftEitherMB :: MonadBase m => Either ContextualError a -> m a
liftEitherMB eerra = case eerra of
                       Left err -> failE $ err
                       Right x  -> return x

-- | Fail (but allow continuation?) if predicate does not hold
check :: MonadBase m => Bool -> ContextualError -> m ()
check b err = if b then return () else failEC () err

-- | Fail (but allow continuation?) if monadic computation evaluates to False
checkM :: MonadBase m => m Bool -> ContextualError -> m ()
checkM mb err = mb >>= flip check err
