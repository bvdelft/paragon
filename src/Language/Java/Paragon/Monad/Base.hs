{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
-- | This module contains the implementation of the BaseM monad, the monad
-- that is at the bottom of our monad stack and adds error-handling and
-- unique number generation on top of IO.
-- The module also contains some general monadic functions / combinators.
module Language.Java.Paragon.Monad.Base 
  (
    BaseM, runBaseM,
    raiseErrors,

    MonadIO(..), MonadBase(..), 
    liftEitherMB, tryCatch,
    getFlags,

    getFreshInt, getErrCtxt,

    check, checkM
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State (StateT(..),runStateT,modify,get)

import Data.Maybe

import Language.Java.Paragon.Error
import Language.Java.Paragon.Flags
import Language.Java.Paragon.Monad.Uniq

-- | The base monad deals adds error handling and unique number generation to IO
-- The error-handling is somewhat involved because we assume the existence of
-- both /fatal/ and /non-fatal/ errors: Errors are accumulated as state,
-- and the Maybe monad is used to short-circuit computation in case of fatal
-- errors. If a non-fatal error occurs, the compuation continues with a Just
-- value, but the list of errors is extended.
newtype BaseM a = 
  BaseM ([ErrorContext] -> Uniq -> [Flag] -> StateT [Error] IO (Maybe a))

instance Monad BaseM where
  return x      = BaseM $ \_ _ _-> return . Just $ x
  BaseM f >>= k = 
      BaseM $ \ec u fl -> 
          do esa <- f ec u fl
             case esa of
               Nothing -> return Nothing
               Just a ->
                 let BaseM g = k a
                  in g ec u fl

  -- Provided for the sake of completeness;
  -- failE and failEC should be used instead
  fail err = BaseM $ \ec _ _ -> do
               modify (\s -> ( undefinedError err ec) : s)
               return Nothing

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
  liftIO ioa = BaseM $ \_ _ _-> StateT (\s -> do x <- ioa
                                                 return (Just x, s))
                               -- ^Just execute the action without touching
                               -- the (error) state

class MonadIO m => MonadBase m where
  -- | Lift BaseM computations to member of the MonadBase class
  liftBase :: BaseM a -> m a
  -- | Run computation with modified error context
  withErrCtxt :: ErrorContext -> m a -> m a
  -- | Try computation, returning the result in case of success or the errors
  tryM :: m a -> m (Either [Error] a)
  -- | Fatal error: abort computation (like fail, but on Error, not String)
  failE :: ContextFreeError -> m a       
  -- | Non-fatal error: continue computation from provided default value
  failEC :: a -> ContextFreeError -> m a

instance MonadBase BaseM where
  liftBase = id
  withErrCtxt ectxt (BaseM f) = BaseM $ \ec u fl -> f (ec ++ [ectxt]) u fl

  -- Construct Either value based on error state
  -- Note that tryM does not distinguish between fatal and non-fatal errors:
  -- If an error occured, we return that rather than any computation result
  tryM (BaseM f) = BaseM $ \ec u fl -> do
                           olderr <- get     -- store collected errs so far
                           modify (\_ -> []) -- start with empty list of err
                           maybeA <- f ec u fl  -- run code
                           err <- get        -- get errs specific to this code
                           modify (\_ -> olderr) -- restore old collection
                           case err of
                             -- No error => The result of the computation is
                             -- of the form Just a
                             -- What we need is a Just (Right a)
                             [] -> return . fmap Right $ maybeA
                             e  -> return . Just . Left $ e
  -- Fatal error => Log error, but Nothing from here
  failE  err     = BaseM $ \ec _ _ -> do modify (\s -> err ec: s)
                                         return Nothing
  -- Non-fatal error => Log error, but continue with Just value
  failEC def err = BaseM $ \ec _ _ -> do modify (\s -> err ec: s)
                                         return . Just $ def

-- | Running a BaseM computation, which will result in either a
-- non-empty list of errors or the result of successfully running
-- the monadic computation
runBaseM :: [Flag] -> BaseM a -> IO (Either [Error] a)
runBaseM flags (BaseM f) = do
  initU <- initUniq
  (a,s) <- runStateT (f [] initU flags) []
  case s of [] -> return . Right . fromJust $ a -- No error => Just value
            e  -> return . Left $ e

-- | Abort computation if any non-fatal errors have been logged via failEC
-- The idea is that non-fatal errors collected with failEC do become fatal
-- in later stages of compilation, so we need a mechanism to fail based on
-- previous errors
raiseErrors :: BaseM ()
raiseErrors = BaseM $ \_ _ _->
  do err <- get
     case err of
       [] -> return $ Just ()
       _e -> return Nothing

-- | Access the unique number generation context of a MonadBase monad
-- It should never be necessary to call this, use getFreshInt to get a new
-- number directly. (This function is therefore not exported.)
getUniqRef :: MonadBase m => m Uniq
getUniqRef = liftBase $ BaseM $ \_ u _ -> return . Just $ u

-- | Get the flags passed to the compiler
getFlags :: MonadBase m => m [Flag]
getFlags = liftBase $ BaseM $ \_ _ fl -> return . Just $ fl

-- | Get a fresh, unused number from the unique number generator
getFreshInt :: MonadBase m => m Int
getFreshInt = do
  u <- getUniqRef
  liftIO $ getUniq u

-- | Access the ErrCtxt environment
getErrCtxt :: MonadBase m => m [ErrorContext]
getErrCtxt = liftBase $ BaseM $ \ec _ _ -> return . Just $ ec

-- | Try first argument. If and only it fails, execute catch computation 
-- given as second argument.
tryCatch :: MonadBase m => m a -> ([Error] -> m a) -> m a
tryCatch tr ctch = do esa <- tryM tr
                      case esa of
                        Right a -> return a
                        Left err -> ctch err

-- | Lift Either value into monad by mapping Left to fail and Right to return
liftEitherMB :: MonadBase m => Either ContextFreeError a -> m a
liftEitherMB eerra = case eerra of
                       Left err -> failE $ err
                       Right x  -> return x

-- | Fail (but allow continuation?) if predicate does not hold
check :: MonadBase m => Bool -> ContextFreeError -> m ()
check b err = if b then return () else failEC () err

-- | Fail (but allow continuation?) if monadic computation evaluates to False
checkM :: MonadBase m => m Bool -> ContextFreeError -> m ()
checkM mb err = mb >>= flip check err
