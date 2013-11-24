-- | Monad-independent helper functions.
module Language.Java.Paragon.Monad.Helpers 
  (
    -- * Helpers
    liftEither
  , ignore
  , orElse
  , orM
  , anyM
  , maybeM
  , withFold
  , withFoldMap
  ) where

-- | Lift Either value into monad by mapping Left to fail and Right to return
liftEither :: Monad m => Either String a -> m a
liftEither esa = case esa of
                   Left err -> fail err
                   Right x  -> return x

-- | Explicitly ignore the result of the computation
ignore :: Monad m => m a -> m ()
ignore = (>> return ())

-- | Get out of the Maybe monad by providing functionality for the Just- and
-- default functionality for the Nothing case
orElse :: Monad m => m (Maybe a) -> m a -> m a
orElse monma mona = do
  ma <- monma
  case ma of
    Just a -> return a
    Nothing -> mona

infixr 3 `orElse`

-- | Monadic non-strict disjunction
orM :: Monad m => m Bool -> m Bool -> m Bool
orM mba mbb = do
  ba <- mba
  if ba then return True else mbb

-- | Monadic non-strict any (list disjunction)
anyM :: Monad m => [m Bool] -> m Bool
anyM [] = return False
anyM (m:ms) = m `orM` anyM ms

-- | Run given computation if given value is Just something.
-- Otherwise just return
maybeM :: Monad m => Maybe a -> (a -> m ()) -> m ()
maybeM ma f = maybe (return ()) f ma

-- | Compose given list of monadic functions
withFold :: Monad m => [m a -> m a] -> m a -> m a
withFold = foldr (.) id

-- | Turn list into sequence of monadic computations that is then composed
withFoldMap :: Monad m => (a -> m b -> m b) -> [a] -> m b -> m b
withFoldMap f = withFold . map f
