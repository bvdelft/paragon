module Language.Java.Paragon.NameResolution.Resolvers.Names
  (
    -- * Resolver
    rnName
  ) where

import qualified Data.Map as Map

import Language.Java.Paragon.Interaction
import Language.Java.Paragon.Monad.Base
import Language.Java.Paragon.Monad.NameRes
import Language.Java.Paragon.Monad.PiReader
import Language.Java.Paragon.SrcPos
import Language.Java.Paragon.Syntax

import Language.Java.Paragon.NameResolution.Errors
import Language.Java.Paragon.NameResolution.Helpers

thisModule :: String
thisModule = libraryBase ++ ".NameResolution.Resolvers.Name"

-- | The function that actually resolves a name.
rnName :: Resolve Name
rnName n = do
  case namePrefix n of
    -- No prefix => resolve through expansion
    Nothing  -> do expn <- getExpansion
                   case Map.lookup (idIdent (nameId n), nameType n) expn of
                     Just nrAction -> do (mPre, resNt) <- liftEither nrAction
                                         return $ n { nameType = resNt, namePrefix = mPre }
                     -- No expansion => resolve as package (??) TODO
                     Nothing       -> do failEC n $ unresolvedName n (nameAnn n)    
    -- Prefix => Resolve prefix
    Just pre -> do preRaw <- rnName pre
                   let pre' = if nameType preRaw == ExpOrLockName
                               then preRaw { nameType = ExpName } else preRaw
                   let name = n { namePrefix = Just $ pre' }
                   -- Prefix resolved, continue with current name
                   case nameType n of
                     AmbigName        -> resolveAmbig pre' name
                     PkgName          -> withPackage name $ return name
                     PkgOrTypeName    -> resolvePkgOrType name
                     TypeName         -> withTypeCurr name $ return name
                     ExpName          -> resolveCandExp pre' name
                     ExpOrLockName    -> resolveCandExp pre' name
                     MethodName       -> resolveCandMethod pre' name
                     MethodOrLockName -> resolveCandMethod pre' name
                     LockName         -> resolveLock pre' name

-- | Resolve ambiguous names based on the prefix.
resolveAmbig :: Name SrcSpan -> Resolve Name
resolveAmbig pre n = do
  case nameType pre of
    -- Package => this is either a type or pkg
    -- TODO: should postpone pkg-existence check?
    PkgName    -> withPackage pre $ do
                    let tName = n { nameType = TypeName }
                    isType <- checkForType tName
                    return $ if isType then tName else tName { nameType = PkgName }
    -- Type => inner types are not supported, so this has to be an expression
    TypeName   -> return $ n { nameType = ExpName }
    -- Expression => inner types are not supported, so also an expression
    ExpName    -> return $ n { nameType = ExpName }
    -- Other name types can't have prefixes
    LockName   -> failEC n $ illegalDeref "lock" (unparsePrint n) (nameAnn n)
    MethodName -> failEC n $ illegalDeref "method" (unparsePrint n) (nameAnn n)
    MethodOrLockName -> failEC n $ illegalDeref "method or lock" 
                                     (unparsePrint n) (nameAnn n)
    -- Other names are not expected
    _          -> panic (thisModule ++ ".resolveAmbig") $
                    "Unexpected name: " ++ show n

-- | Resolve a name that can be either a package or type, by simply checking
-- for each's existence.
resolvePkgOrType :: Resolve Name
resolvePkgOrType n = do
  let tName = n { nameType = TypeName }
  isType <- checkForType tName
  if isType
   then return tName
   else do
     let pName = n { nameType = PkgName }
     isPkg <- doesPkgExist pName
     if isPkg
      then return pName
      else failEC n $ unresolvedName n (nameAnn n)


-- | Resolves candidate expressions. Possible ambiiguity remains in the case of
-- @ExpOrLockName@ and is left to be resolved in typechecking phase.
resolveCandExp :: Name SrcSpan -> Resolve Name
resolveCandExp pre n = do
  case nameType pre of
    PkgName  -> -- A package cannot contain expressions directly
                failEC n $ exprInPkgError "field, variable or lock" pre n (nameAnn n)
    TypeName -> return n
    ExpName  -> return n
    -- other types are not expected
    _        -> panic (thisModule ++ ".resolveCandExp") $
                  "Unexpected name: " ++ show n

-- | Resolves candidate methods. Possible ambiiguity remains in the case of
-- @MethodOrLockName@ and is left to be resolved in typechecking phase.
resolveCandMethod :: Name SrcSpan -> Resolve Name
resolveCandMethod pre n = do
  case nameType pre of
    PkgName  -> -- A package cannot contain methods/locks directly
                failEC n $ exprInPkgError "method or lock" pre n (nameAnn n)
    TypeName -> return n
    ExpName  -> return n
    -- other types are not expected
    _        -> panic (thisModule ++ ".resolveCandMethod") $
                  "Unexpected name: " ++ show n

-- | Resolves locks. Basically not yet implemented.
resolveLock :: Name SrcSpan -> Resolve Name
resolveLock pre n = do
  case nameType pre of
    PkgName  -> -- A package cannot contain methods/locks directly
                failEC n $ exprInPkgError "lock" pre n (nameAnn n)        
    TypeName -> return n -- TODO: Check here that pi file contains at least one
                         -- member with name (nameId n) which must be a lock.
                         -- .. why not also leave to typechecker?
    ExpName  -> return n
    -- other types are not expected
    _        -> panic (thisModule ++ ".resolveLock") $
                  "Unexpected name: " ++ show n
