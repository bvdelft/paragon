-- | Paragon Abstract Syntax Tree.
module Language.Java.Paragon.Syntax where

-- | Type synonym for top-level AST node.
type AST = CompilationUnit

-- | Identifier data type.
data Id a = Id
  { idAnn  :: a       -- ^ Annotation.
  , idName :: String  -- ^ Identifier's string.
  } deriving (Show, Eq)

-- | Qualified identifier. A period-separated list of identifiers.
data QId a = QId
  { qIdAnn      :: a              -- ^ Annotation.
  , qIdName     :: Id a           -- ^ Identifier.
  , qIdNameType :: NameType       -- ^ Type of the name.
  , qIdPrevName :: Maybe (QId a)  -- ^ Possibly, name part before the period.
  } deriving (Show, Eq)

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
  { pdAnn :: a      -- ^ Annotation.
  , pdId  :: QId a  -- ^ Package identifier.
  } deriving (Show, Eq)

-- | Import declaration.
data ImportDecl a =
    -- | Import a single type.
    -- Example: import java.util.LinkedList;
    SingleTypeImport a (QId a)

    -- | Import all the types contained in a package.
    -- Example: import java.util.*;
  | TypeImportOnDemand a (QId a)

    -- | Static import of a single type.
    -- Example: import static java.lang.Math.PI;
  | SingleStaticImport a (QId a)

    -- | Static import of all members.
    -- Example: import static java.lang.Math.*;
  | StaticImportOnDemand a (QId a)
  deriving (Show, Eq)

-- | Class or interface declaration.
data TypeDecl a = ClassTypeDecl a (ClassDecl a)
                | InterfaceTypeDecl a (InterfaceDecl a)
  deriving (Show, Eq)

-- | Class declaration.
data ClassDecl a = C
  deriving (Show, Eq)

-- | Interface declaration.
data InterfaceDecl a = I
  deriving (Show, Eq)

-- Helper functions

-- | Create a qualified identifier from name type and list of identifiers.
-- Takes a function to combine annotations.
mkQId :: (a -> a -> a) -> NameType -> [Id a] -> QId a
mkQId combine nameType ids = mkQId' (reverse ids)
  where mkQId' [i]    = QId (idAnn i) i nameType Nothing
        mkQId' (i:is) = let pre = mkQId' is
                        in QId (combine (qIdAnn pre) (idAnn i)) i nameType (Just pre)
        mkQId' [] = undefined  -- TODO

