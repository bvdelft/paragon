module Language.Java.Paragon.NameResolution.Helpers
  (
    withPackage
  , withType
  , withTypeCurr
  , checkForType
  ) where

import Language.Java.Paragon.Interaction
import Language.Java.Paragon.Monad.Base
import Language.Java.Paragon.Monad.NameRes
import Language.Java.Paragon.Monad.PiReader
import Language.Java.Paragon.Syntax

import Language.Java.Paragon.NameResolution.Errors

-- | Execute action if given name is a package. Fails with error otherwise
withPackage :: MonadPR m => Name -> m a -> m a
withPackage pkg action = do
  tracePrint $ "withPackage " ++ (unparsePrint pkg)
  isP <- doesPkgExist pkg
  if isP 
   then action
   else failE $ unresolvedName pkg pkg

-- | Execute action if given name is a type
-- Fails with error if argument not a type
withType :: MonadPR m => Name -> m a -> m a
withType typ action = do 
  isType <- doesTypeExist typ
  if isType 
   then action
   else failE $ unresolvedName typ typ

-- | Execute action if given name is a type. Compare to current name first
-- (I.e. to be called only in a context were the current name is a type)
-- Fails with error if argument not a type
withTypeCurr :: Name -> NameRes a -> NameRes a
withTypeCurr typ action = do 
  isT <- checkForType typ
  if isT 
   then action
   else failE $ unresolvedName typ typ

-- | See if type of given name is in scope, by looking at pi-path and
-- comparing to the name of the type that we're currently resolving
checkForType :: Name -> NameRes Bool
checkForType n = do
  n' <- getCurrentName
  -- debugPrint $ "checkForType: " ++ show (n, n')
  if n == n' then return True else doesTypeExist n
