{-# LANGUAGE TemplateHaskell
           , DeriveFunctor
 #-}

-- | Paragon Abstract Syntax Tree. Names.
module Language.Java.Paragon.Syntax.Names where

import Text.PrettyPrint

import Language.Java.Paragon.Interaction (panic, libraryBase, Unparse(..))
import Language.Java.Paragon.SrcPos

import Language.Java.Paragon.Annotated

namesModule :: String
namesModule = libraryBase ++ ".Syntax.Names"

-- | Identifier data type.
data Id a = Id
  { idAnn   :: a       -- ^ Annotation.
  , idIdent :: String  -- ^ Identifier's string.
  } deriving (Show, Eq, Functor, Ord)

-- | Qualified name. A dot-separated list of identifiers.
data Name a = Name
  { nameAnn    :: a               -- ^ Annotation.
  , nameId     :: Id a            -- ^ Identifier.
  , nameType   :: NameType        -- ^ Type of the name.
  , namePrefix :: Maybe (Name a)  -- ^ Possibly, name part before the dot.
  } deriving (Show, Eq, Functor)

instance Unparse (Name a) where
  unparse name =
    case namePrefix name of
      Nothing  -> text $ show (idIdent (nameId name))
      Just pre -> unparse pre <> text "." <> text (show (idIdent (nameId name)))

-- | Types of the names, e.g. expression, method, type etc.
data NameType = ExpName           -- ^ Expression name.
              | MethodName        -- ^ Method name.
              | TypeName          -- ^ Type name.
              | PkgName           -- ^ Package name
              | LockName          -- ^ Lock name.
              | PkgOrTypeName     -- ^ Package or type name.
              | MethodOrLockName  -- ^ Method or lock name.
              | ExpOrLockName     -- ^ Expression or lock name.
              | AmbigName         -- ^ Ambiguous name.
  deriving (Show, Eq, Ord)

$(deriveAnnotatedMany
  [ ''Id
  , ''Name
  ])

-- Name type helpers.

-- | Creates a qualified name from name type and list of identifiers.
-- Takes a function to combine annotations.
mkName :: (a -> a -> a) -> NameType -> [Id a] -> Name a
mkName combine nameT ids = mkName' (reverse ids)
  where mkName' [i]    = Name (idAnn i) i nameT Nothing
        mkName' (i:is) = let pre = mkName' is
                         in Name (combine (nameAnn pre) (idAnn i)) i nameT (Just pre)
        mkName' [] = panic (namesModule ++ ".mkName") "empty list of identifiers"

-- | Transforms a qualified name to list of identifiers.
flattenName :: Name a -> [Id a]
flattenName name = reverse (flattenName' name)
  where flattenName' (Name _ i _ mPre) = i : maybe [] flattenName' mPre

-- Create qualified names of different name types from list of identifiers.

mkNameSrcSpan :: NameType -> [Id SrcSpan] -> Name SrcSpan
mkNameSrcSpan = mkName combineSrcSpan

expName :: [Id SrcSpan] -> Name SrcSpan
expName = mkNameSrcSpan ExpName

typeName :: [Id SrcSpan] -> Name SrcSpan
typeName = mkNameSrcSpan TypeName

pkgName :: [Id SrcSpan] -> Name SrcSpan
pkgName = mkNameSrcSpan PkgName

lockName :: [Id SrcSpan] -> Name SrcSpan
lockName = mkNameSrcSpan LockName

pkgOrTypeName :: [Id SrcSpan] -> Name SrcSpan
pkgOrTypeName = mkNameSrcSpan PkgOrTypeName

expOrLockName :: [Id SrcSpan] -> Name SrcSpan
expOrLockName = mkNameSrcSpan ExpOrLockName

ambigName :: [Id SrcSpan] -> Name SrcSpan
ambigName = mkNameSrcSpan AmbigName

