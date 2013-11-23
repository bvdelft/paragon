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
import Control.Monad.Trans.State (StateT(..),runStateT,modify,get,put)

import Data.Maybe

import Language.Java.Paragon.Error
import Language.Java.Paragon.Flags

-- | State of the base monad, as a record for easier extensibility.
data BaseState = BaseState
  { -- | Accumulates the errors.
    bsErrors       :: [Error]
    -- | For generating fresh integers.
  , bsNextUniq     :: Int
    -- | Current context (class, method) to identify where errors are raised.
  , bsErrorContext :: [ErrorContext]
    -- | The flags given to the compilation.
  , bsFlags        :: [Flag]
  }

-- | The base monad deals adds error handling and unique number generation to IO
-- The error-handling is somewhat involved because we assume the existence of
-- both /fatal/ and /non-fatal/ errors: Errors are accumulated as state,
-- and the Maybe monad is used to short-circuit computation in case of fatal
-- errors. If a non-fatal error occurs, the compuation continues with a Just
-- value, but the list of errors is extended.
newtype BaseM a = BaseM (StateT BaseState IO (Maybe a))

instance Monad BaseM where
  return x      = BaseM $ return . Just $ x
  BaseM f >>= k = 
      BaseM $
          do maybeA <- f
             case maybeA of
               Nothing -> return Nothing  -- Fatal error occurred, short-circuit
               Just a ->
                 let BaseM g = k a
                  in g

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
  liftIO ioa = BaseM $ StateT (\s -> do x <- ioa
                                        return (Just x, s))

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

  -- Run the subcomputation in an extended error context, then put the context
  -- back to the way it was.
  withErrCtxt ectxt (BaseM f) = BaseM $
    do oldSt <- get
       modify $ \st -> st {  bsErrorContext = (bsErrorContext st) ++ [ectxt] }
       a <- f
       modify $ \st -> st {  bsErrorContext = (bsErrorContext oldSt) }
       return a

  -- Construct Either value based on error state
  -- Note that tryM does not distinguish between fatal and non-fatal errors:
  -- If an error occured, we return that rather than any computation result
  tryM (BaseM f) = BaseM $
    do st <- get
       put $ st { bsErrors = [] } -- start with try-block empty error list
       maybeA <- f                -- run code of try block
       nSt <- get                 -- get errors from try block
       put $ nSt { bsErrors = bsErrors st}  -- restore original errors
       case bsErrors nSt of
         [] -> return . fmap Right $ maybeA  -- No errors, return result
         e  -> return . Just . Left $ e      -- Return errors
        
  -- Fatal error => Log error, but Nothing from here
  failE  err     = BaseM $
    do modify (\s -> s { bsErrors = err (bsErrorContext s) : bsErrors s})
       return Nothing
       
  -- Non-fatal error => Log error, but continue with Just value
  failEC def err = BaseM $
    do modify (\s -> s { bsErrors = err (bsErrorContext s) : bsErrors s})
       return . Just $ def

-- | Running a BaseM computation, which will result in either a
-- non-empty list of errors or the result of successfully running
-- the monadic computation
runBaseM :: [Flag] -> BaseM a -> IO (Either [Error] a)
runBaseM flags (BaseM f) = do
  (a,s) <- runStateT f (BaseState { bsErrors       = []
                                  , bsErrorContext = []
                                  , bsNextUniq     = 0
                                  , bsFlags        = flags
                                  } )
  case bsErrors s of
    [] -> return . Right . fromJust $ a -- No error => Just value
    e  -> return . Left $ e

-- | Abort computation if any non-fatal errors have been logged via failEC
-- The idea is that non-fatal errors collected with failEC do become fatal
-- in later stages of compilation, so we need a mechanism to fail based on
-- previous errors
raiseErrors :: BaseM ()
raiseErrors = BaseM $
  do st <- get
     case bsErrors st of
       [] -> return $ Just ()
       _e -> return Nothing

-- | Get a fresh, unused number from the unique number generator
getFreshInt :: MonadBase m => m Int
getFreshInt = liftBase $ BaseM $ do
  st <- get
  put $ st { bsNextUniq = bsNextUniq st + 1}
  return $ Just (bsNextUniq st)

-- | Get the flags passed to the compiler
getFlags :: MonadBase m => m [Flag]
getFlags = liftBase $ BaseM $ get >>= (return . Just . bsFlags)

-- | Access the ErrCtxt environment
getErrCtxt :: MonadBase m => m [ErrorContext]
getErrCtxt = liftBase $ BaseM $ get >>= (return . Just . bsErrorContext)

-- | Try first argument. If and only it fails, execute catch computation 
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
