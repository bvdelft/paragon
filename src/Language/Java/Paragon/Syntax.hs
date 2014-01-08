{-# LANGUAGE TemplateHaskell
           , DeriveFunctor
 #-}

-- | Paragon Abstract Syntax Tree.
module Language.Java.Paragon.Syntax
  (
    module Language.Java.Paragon.Syntax
  , module Language.Java.Paragon.Annotated
  ) where

import Language.Java.Paragon.Annotated
import Language.Java.Paragon.Interaction (panic, libraryBase)
import Language.Java.Paragon.Unparse (Unparse(..))
import Text.PrettyPrint

syntaxModule :: String
syntaxModule = libraryBase ++ ".Syntax"

-- | Type synonym for top-level AST node.
type AST = CompilationUnit

-- | Identifier data type.
data Id a = Id
  { idAnn   :: a       -- ^ Annotation.
  , idIdent :: String  -- ^ Identifier's string.
  } deriving (Show, Eq)

-- | Qualified name. A period-separated list of identifiers.
data Name a = Name
  { nameAnn    :: a               -- ^ Annotation.
  , nameId     :: Id a            -- ^ Identifier.
  , nameType   :: NameType        -- ^ Type of the name.
  , namePrefix :: Maybe (Name a)  -- ^ Possibly, name part before the period.
  } deriving (Show, Eq)

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
              | ExprOrLockName    -- ^ Expression or lock name.
              | AmbigName         -- ^ Ambiguous name.
  deriving (Show, Eq)

-- | Compilation unit.
data CompilationUnit a = CompilationUnit
  { cuAnn         :: a                      -- ^ Annotation.
  , cuPkgDecl     :: Maybe (PackageDecl a)  -- ^ Package declaration.
  , cuImportDecls :: [ImportDecl a]         -- ^ Import declarations.
  , cuTypeDecls   :: [TypeDecl a]           -- ^ Type declarations.
  } deriving (Show, Eq)

-- | Package declaration.
data PackageDecl a = PackageDecl
  { pdAnn  :: a       -- ^ Annotation.
  , pdName :: Name a  -- ^ Package name.
  } deriving (Show, Eq)

-- | Import declaration.
data ImportDecl a =
    -- | Import a single type.
    -- Example: import java.util.LinkedList;
    SingleTypeImport a (Name a)

    -- | Import all the types contained in a package.
    -- Example: import java.util.*;
  | TypeImportOnDemand a (Name a)

    -- | Static import of a single type.
    -- Example: import static java.lang.Math.PI;
  | SingleStaticImport a (Name a)

    -- | Static import of all members.
    -- Example: import static java.lang.Math.*;
  | StaticImportOnDemand a (Name a)
  deriving (Show, Eq)

-- | Class or interface declaration.
data TypeDecl a = ClassTypeDecl a (ClassDecl a)
                | InterfaceTypeDecl a (InterfaceDecl a)
  deriving (Show, Eq)

-- | Class declaration.
data ClassDecl a = ClassDecl
  { cdAnn        :: a                    -- ^ Annotation.
  , cdModifiers  :: [Modifier a]         -- ^ List of modifiers.
  , cdId         :: Id a                 -- ^ Class identifier.
  , cdTypeParams :: [TypeParam a]        -- ^ List of type parameters.
  , cdSuperClass :: Maybe (ClassType a)  -- ^ Super class if the class has one.
  , cdInterfaces :: [ClassType a]        -- ^ List of interfaces it implements.
  , cdBody       :: ClassBody a          -- ^ Class body.
  } deriving (Show, Eq)

-- | Interface declaration.
data InterfaceDecl a = InterfaceDecl
  { intdAnn        :: a                -- ^ Annotation.
  , intdModifiers  :: [Modifier a]     -- ^ List of modifiers.
  , intdId         :: Id a             -- ^ Interface identifier.
  , intdTypeParams :: [TypeParam a]    -- ^ List of type parameters.
  , intdInterfaces :: [ClassType a]    -- ^ List of interfaces it extends.
  , intdBody       :: InterfaceBody a  -- ^ Interface body.
  } deriving (Show, Eq)

-- | Modifiers for declarations.
data Modifier a
  = Public    a     -- ^ public
  | Protected a     -- ^ protected
  | Private   a     -- ^ private
  | Static    a     -- ^ static
  | Abstract  a     -- ^ abstract
  | Final     a     -- ^ final
  | Native    a     -- ^ native
  | Synchronized a  -- ^ synchronized
  | Transient a     -- ^ transient
  | Volatile  a     -- ^ volatile
  | StrictFP  a     -- ^ strictfp
  -- Paragon specific
  | Typemethod a
  | Reflexive  a
  | Transitive a
  | Symmetric  a
  | Readonly   a
  | Notnull    a
  deriving (Show, Eq, Functor)

data TypeParam a = TP
  deriving (Show, Eq)

data ClassType a = CT
  deriving (Show, Eq)

data ClassBody a = CB
  deriving (Show, Eq)

data InterfaceBody a = IB
  deriving (Show, Eq)

-- Helper functions

-- | Creates a qualified name from name type and list of identifiers.
-- Takes a function to combine annotations.
mkName :: (a -> a -> a) -> NameType -> [Id a] -> Name a
mkName combine nameT ids = mkName' (reverse ids)
  where mkName' [i]    = Name (idAnn i) i nameT Nothing
        mkName' (i:is) = let pre = mkName' is
                         in Name (combine (nameAnn pre) (idAnn i)) i nameT (Just pre)
        mkName' [] = panic (syntaxModule ++ ".mkName") "empty list of identifiers"

$(deriveAnnotatedMany [''Modifier])

