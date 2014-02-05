module Language.Java.Paragon.NameResolution.Expansion
  ( 
    -- * The @Expansion@ mapping
    Expansion
    -- * Helper expansion builders
  , mkPkgExpansion
  , mkPkgExpansionWithPrefix
  , mkTypeExpansion
  , mkTypeExpansionWithPrefix
  , mkExpExpansion
  , mkExpExpansionWithPrefix
  , mkLockExpansion
  , mkLockExpansionWithPrefix
  , mkMethodExpansion
  , mkMethodExpansionWithPrefix
    -- * Combining expansions
  , expansionUnionLeftB
  , expansionUnion
  , emptyExpansion
  ) where

import qualified Data.Map as Map

import Language.Java.Paragon.Error
import Language.Java.Paragon.SrcPos (SrcSpan, defaultSpan)
import Language.Java.Paragon.Syntax

import Language.Java.Paragon.NameResolution.Errors

-- | Mapping names to their resolved versions.
-- A combination of string and the possible nameType, is mapped to a specific 
-- nameType and the prefix under which it has this nameType.
type Expansion =
    Map.Map (String, NameType) -- ^ (Partially) unresolved 
            (Either ContextualError (Maybe (Name SrcSpan), NameType)) 
            -- ^ Resolved NameType /or/ an error due to ambiguity

-- | Create an expansion for a package name.
mkPkgExpansion :: String -> Expansion
mkPkgExpansion = mkPkgExpansionWithPrefix Nothing

-- | Create an expansion for a package name for the specified prefix.
-- I.e., the resulting expansion states that the provided name has type @PName@
-- when occuring after the provided prefix.
mkPkgExpansionWithPrefix :: Maybe (Name SrcSpan) -> String -> Expansion
mkPkgExpansionWithPrefix mPre name =
  Map.fromList [((name, PkgName      ), return (mPre, PkgName)),
                ((name, PkgOrTypeName), return (mPre, PkgName)),
                ((name, AmbigName    ), return (mPre, PkgName))]

-- | Create an expansion for a type name.
mkTypeExpansion :: String -> Expansion
mkTypeExpansion = mkTypeExpansionWithPrefix Nothing

-- | Create an expansion for a type name for the specified prefix.
mkTypeExpansionWithPrefix :: Maybe (Name SrcSpan) -> String -> Expansion
mkTypeExpansionWithPrefix mPre name =
  Map.fromList [((name, TypeName     ), return (mPre, TypeName)),
                ((name, PkgOrTypeName), return (mPre, TypeName)),
                ((name, AmbigName    ), return (mPre, TypeName))]

-- | Create an expansion for an expression name.
mkExpExpansion :: String -> Expansion
mkExpExpansion = mkExpExpansionWithPrefix Nothing

-- | Create an expansion for an expression name for the specified prefix.
mkExpExpansionWithPrefix :: Maybe (Name SrcSpan) -> String -> Expansion
mkExpExpansionWithPrefix mPre name =
  Map.fromList [((name, ExpName      ), return (mPre, ExpName)),
                ((name, ExpOrLockName), return (mPre, ExpName)),
                ((name, AmbigName    ), return (mPre, ExpName))]

-- | Create an expansion for a method name.
mkMethodExpansion :: String -> Expansion
mkMethodExpansion = mkMethodExpansionWithPrefix Nothing

-- | Create an expansion for a method name for the specified prefix.
mkMethodExpansionWithPrefix :: Maybe (Name SrcSpan) -> String -> Expansion
mkMethodExpansionWithPrefix mPre name =
  Map.fromList [((name, MethodName      ), return (mPre, MethodName)),
                ((name, MethodOrLockName), return (mPre, MethodName)),
                ((name, AmbigName       ), return (mPre, MethodName))]

-- | Create an expansion for a lock name.
mkLockExpansion :: String -> Expansion
mkLockExpansion = mkLockExpansionWithPrefix Nothing

-- | Create an expansion for a lock name for the specified prefix.
mkLockExpansionWithPrefix :: Maybe (Name SrcSpan) -> String -> Expansion
mkLockExpansionWithPrefix mPre name =
  Map.fromList [((name, LockName        ), return (mPre, LockName)),
                ((name, MethodOrLockName), return (mPre, LockName)),
                ((name, ExpOrLockName   ), return (mPre, LockName)),
                ((name, AmbigName       ), return (mPre, LockName))]

-- | Unions expansions using the default Map.union which is left-biased.
expansionUnionLeftB :: [Expansion] -> Expansion
expansionUnionLeftB = foldr Map.union Map.empty

-- | Union maps, stores an error when encountering the same name multiple time,
-- ambiguously.
expansionUnion :: [Expansion] -> Expansion
expansionUnion = foldl (Map.unionWithKey foldC) Map.empty
  where foldC (i,_) res1 res2 = do 
          (mPre1, _) <- res1
          (mPre2, _) <- res2
          if (mPre1 /= mPre2)
           then Left $ ambiguousName i mPre1 mPre2 defaultSpan
           else res1

-- | Empty expansion
emptyExpansion :: Expansion
emptyExpansion = Map.empty
