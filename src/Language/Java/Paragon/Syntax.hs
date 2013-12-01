-- | Paragon Abstract Syntax Tree.
module Language.Java.Paragon.Syntax where

-- | Type synonym for top-level AST node.
type AST = CompilationUnit

-- | Identifier data type.
data Id a = Id
  { idAnn  :: a      -- ^ Annotation.
  , idName :: String -- ^ Identifier's string.
  } deriving Show

-- | Qualified identifier. A period-separated list of identifiers.
data QId a = QId
  { qIdAnn      :: a             -- ^ Annotation.
  , qIdName     :: Id a          -- ^ Identifier.
  , qIdNameType :: NameType      -- ^ Type of the name.
  , qIdPrevName :: Maybe (QId a) -- ^ Possibly, name part before the period.
  } deriving Show

-- | Types of the names, e.g. expression, method, type etc.
data NameType = ExpName
              | MethodName
              | TypeName
              | PkgName
              | LockName
              | PkgOrTypeName
              | MethodOrLockName
              | ExprOrLockName
              | AmbigName
  deriving Show

-- | Compilation unit.
data CompilationUnit a = CompilationUnit
  { cuAnn         :: a                     -- ^ Annotation.
  , cuPkgDecl     :: Maybe (PackageDecl a) -- ^ Package declaration.
  , cuImportDecls :: [ImportDecl a]        -- ^ Import declarations.
  , cuTypeDecls   :: [TypeDecl a]          -- ^ Type declarations.
  } deriving Show

-- | Package declaration.
data PackageDecl a = PackageDecl
  { pdAnn :: a     -- ^ Annotation.
  , pdId  :: QId a -- ^ Package identifier.
  } deriving Show

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
  deriving Show

-- | Class or interface declaration.
data TypeDecl a = ClassTypeDecl a (ClassDecl a)
                | InterfaceTypeDecl a (InterfaceDecl a)
  deriving Show

-- | Class declaration.
data ClassDecl a = C
  deriving Show

-- | Interface declaration.
data InterfaceDecl a = I
  deriving Show

-- Helper functions

-- | Create a qualified identifier from name type and list of identifiers.
-- Takes a function to combine annotations.
mkQId :: (a -> a -> a) -> NameType -> [Id a] -> QId a
mkQId combine nameType ids = mkQId' (reverse ids)
  where mkQId' [i]    = QId (idAnn i) i nameType Nothing
        mkQId' (i:is) = let pre = mkQId' is
                        in QId (combine (qIdAnn pre) (idAnn i)) i nameType (Just pre)
        mkQId' [] = undefined -- TODO

