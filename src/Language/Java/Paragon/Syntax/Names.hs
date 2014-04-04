{-# LANGUAGE TemplateHaskell
           , DeriveFunctor
           , DeriveDataTypeable
 #-}

-- | Paragon Abstract Syntax Tree. Names.
module Language.Java.Paragon.Syntax.Names where

import Data.Data

import Text.PrettyPrint

import Language.Java.Paragon.Interaction.Headers (libraryBase)
import Language.Java.Paragon.Interaction.Panic (panic)
import Language.Java.Paragon.Interaction.Unparse (Unparse(..))
import Language.Java.Paragon.SrcPos

import Language.Java.Paragon.Annotation
import Language.Java.Paragon.Annotated

namesModule :: String
namesModule = libraryBase ++ ".Syntax.Names"

-- | Identifier data type.
data Id = Id
  { idAnn   :: Annotation  -- ^ Annotation.
  , idIdent :: String      -- ^ Identifier's string.
  } deriving (Data, Typeable, Show, Eq, Ord)

instance Annotated Id where
  getAnn = idAnn
  setAnn a x = x { idAnn = a }

-- | Qualified name. A dot-separated list of identifiers.
data Name = Name
  { nameAnn    :: Annotation  -- ^ Annotation.
  , nameId     :: Id          -- ^ Identifier.
  , nameType   :: NameType    -- ^ Type of the name.
  , namePrefix :: Maybe Name  -- ^ Possibly, name part before the dot.
  } deriving (Data, Typeable, Show, Eq, Ord)

instance Annotated Name where
  getAnn = nameAnn
  setAnn a x = x { nameAnn = a }

instance Unparse Name where
  unparse name =
    case namePrefix name of
      Nothing  -> text $ idIdent (nameId name)
      Just pre -> unparse pre <> text "." <> text (idIdent (nameId name))

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
  deriving (Typeable, Data, Show, Eq, Ord)

-- Name type helpers.

-- | Creates a qualified name from name type and list of identifiers.
-- Takes a function to combine annotations.
mkName :: (Annotation -> Annotation -> Annotation) -> NameType -> [Id] -> Name
mkName combine nameT ids = mkName' (reverse ids)
  where mkName' [i]    = Name (getAnn i) i nameT Nothing
        mkName' (i:is) = let pre = mkName' is
                         in Name (combine (getAnn pre) (getAnn i)) i nameT (Just pre)
        mkName' [] = panic (namesModule ++ ".mkName") "empty list of identifiers"

-- | Transforms a qualified name to list of identifiers.
flattenName :: Name -> [Id]
flattenName name = reverse (flattenName' name)
  where flattenName' (Name _ i _ mPre) = i : maybe [] flattenName' mPre

-- Create qualified names of different name types from list of identifiers.

mkNameSrcSpan :: NameType -> [Id] -> Name
mkNameSrcSpan = mkName (\a -> \b -> 
  a { annSrcSpan = combineSrcSpan (annSrcSpan a) (annSrcSpan b) })

expName :: [Id] -> Name
expName = mkNameSrcSpan ExpName

typeName :: [Id] -> Name
typeName = mkNameSrcSpan TypeName

pkgName :: [Id] -> Name
pkgName = mkNameSrcSpan PkgName

lockName :: [Id] -> Name
lockName = mkNameSrcSpan LockName

pkgOrTypeName :: [Id] -> Name
pkgOrTypeName = mkNameSrcSpan PkgOrTypeName

expOrLockName :: [Id] -> Name
expOrLockName = mkNameSrcSpan ExpOrLockName

ambigName :: [Id] -> Name
ambigName = mkNameSrcSpan AmbigName

-- | Assumes a non-empty list. The last element of the list is labelled to be a
-- TypeName, the preceding elements are PkgOrTypeNames.
qualifiedTypeName :: [Id] -> Name
qualifiedTypeName [x] = typeName [x]
qualifiedTypeName ids = combineNames [ pkgOrTypeName (init ids)
                                     , typeName [last ids]
                                     ]

-- | Combines names, making each preceding element a prefix of the next. Expects
--  a non-empty list of names.
combineNames :: [Name] -> Name
combineNames names = combineNames' (reverse names)
  where combineNames' []     = panic (namesModule ++ ".combineNames") $ "Empty list provided."
        combineNames' [n]    = n
        combineNames' (n:ns) = 
          let pre = combineNames' ns
              newSpan = combineSrcSpan (annSrcSpan $ getAnn n) (annSrcSpan $ getAnn pre)
              newAnn  = emptyAnnotation { annSrcSpan = newSpan }
          in n { namePrefix = Just pre, nameAnn = newAnn }
