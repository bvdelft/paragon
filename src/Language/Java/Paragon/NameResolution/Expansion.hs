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
  , expansionUnion
  , emptyExpansion
  ) where

import Data.Map (Map, fromList, union, empty)

import Language.Java.Paragon.SrcPos (SrcSpan)
import Language.Java.Paragon.Syntax

-- | Mapping names to their resolved versions. Note that the srcspan does not
-- play any role in the expansion but is only there for type correctness.
type Expansion =
    Map (String, NameType)  -- ^ (Partially) unresolved NameType.
        (Either String (Maybe (Name SrcSpan), NameType))  -- ^ Resolved NameType
        -- TODO: what is Left used for?

-- | Create an expansion for a package name.
mkPkgExpansion :: String -> Expansion
mkPkgExpansion = mkPkgExpansionWithPrefix Nothing

-- |  Create an expansion for a package name for the specified prefix.
-- I.e., the resulting expansion states that the provided name has type @PName@
-- when occuring after the provided prefix.
mkPkgExpansionWithPrefix :: Maybe (Name SrcSpan) -> String -> Expansion
mkPkgExpansionWithPrefix mPre name =
  fromList [((name, PkgName      ), return (mPre, PkgName)),
            ((name, PkgOrTypeName), return (mPre, PkgName)),
            ((name, AmbigName    ), return (mPre, PkgName))]

-- | Create an expansion for a type name.
mkTypeExpansion :: String -> Expansion
mkTypeExpansion = mkTypeExpansionWithPrefix Nothing

-- |  Create an expansion for a type name for the specified prefix.
mkTypeExpansionWithPrefix :: Maybe (Name SrcSpan) -> String -> Expansion
mkTypeExpansionWithPrefix mPre name =
  fromList [((name, TypeName     ), return (mPre, TypeName)),
            ((name, PkgOrTypeName), return (mPre, TypeName)),
            ((name, AmbigName    ), return (mPre, TypeName))]

-- | Create an expansion for an expression name.
mkExpExpansion :: String -> Expansion
mkExpExpansion = mkExpExpansionWithPrefix Nothing

-- |  Create an expansion for an expression name for the specified prefix.
mkExpExpansionWithPrefix :: Maybe (Name SrcSpan) -> String -> Expansion
mkExpExpansionWithPrefix mPre name =
  fromList [((name, ExpName      ), return (mPre, ExpName)),
            ((name, ExpOrLockName), return (mPre, ExpName)),
            ((name, AmbigName    ), return (mPre, ExpName))]

-- | Create an expansion for a method name.
mkMethodExpansion :: String -> Expansion
mkMethodExpansion = mkMethodExpansionWithPrefix Nothing

-- |  Create an expansion for a method name for the specified prefix.
mkMethodExpansionWithPrefix :: Maybe (Name SrcSpan) -> String -> Expansion
mkMethodExpansionWithPrefix mPre name =
  fromList [((name, MethodName      ), return (mPre, MethodName)),
            ((name, MethodOrLockName), return (mPre, MethodName)),
            ((name, AmbigName       ), return (mPre, MethodName))]

-- | Create an expansion for a lock name.
mkLockExpansion :: String -> Expansion
mkLockExpansion = mkLockExpansionWithPrefix Nothing

-- |  Create an expansion for a lock name for the specified prefix.
mkLockExpansionWithPrefix :: Maybe (Name SrcSpan) -> String -> Expansion
mkLockExpansionWithPrefix mPre name =
  fromList [((name, LockName        ), return (mPre, LockName)),
            ((name, MethodOrLockName), return (mPre, LockName)),
            ((name, ExpOrLockName   ), return (mPre, LockName)),
            ((name, AmbigName       ), return (mPre, LockName))]

-- | Convert list of expansions to a single expansion by taking the union.
expansionUnion :: [Expansion] -> Expansion
expansionUnion = foldr union empty

-- | Empty expansion
emptyExpansion :: Expansion
emptyExpansion = empty
