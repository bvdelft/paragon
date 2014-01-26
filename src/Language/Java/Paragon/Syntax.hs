{-# LANGUAGE TemplateHaskell
           , DeriveFunctor
 #-}

-- | Paragon Abstract Syntax Tree.
module Language.Java.Paragon.Syntax
  (
    module Language.Java.Paragon.Syntax
  , module Language.Java.Paragon.Annotated
  ) where

import Text.PrettyPrint

import Language.Java.Paragon.Annotated
import Language.Java.Paragon.Interaction (panic, libraryBase)
import Language.Java.Paragon.Unparse (Unparse(..))
import Language.Java.Paragon.SrcPos

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
    SingleTypeImport { impdAnn  :: a       -- ^ Annotation.
                     , impdName :: Name a  -- ^ Type name.
                     }

    -- | Import all the types contained in a package.
    -- Example: import java.util.*;
  | TypeImportOnDemand { impdAnn  :: a       -- ^ Annotation.
                       , impdName :: Name a  -- ^ Package name.
                       }

    -- | Static import of a single type.
    -- Example: import static java.lang.Math.PI;
  | SingleStaticImport { impdAnn  :: a       -- ^ Annotation.
                       , impdName :: Name a  -- ^ Member name.
                       }

    -- | Static import of all members.
    -- Example: import static java.lang.Math.*;
  | StaticImportOnDemand { impdAnn  :: a       -- ^ Annotation.
                         , impdName :: Name a  -- ^ Package name.
                         }
  deriving (Show, Eq)

-- | Class or interface declaration.
data TypeDecl a =
    ClassTypeDecl     { tdAnn       :: a            -- ^ Annotation.
                      , tdClassDecl :: ClassDecl a  -- ^ Class declaration.
                      }
  | InterfaceTypeDecl { tdAnn     :: a                -- ^ Annotation.
                      , tdIntDecl :: InterfaceDecl a  -- ^ Interface declaration.
                      }
  deriving (Show, Eq)

-- | Class declaration.
data ClassDecl a = ClassDecl
  { cdAnn        :: a                    -- ^ Annotation.
  , cdModifiers  :: [Modifier a]         -- ^ Modifiers.
  , cdId         :: Id a                 -- ^ Class identifier.
  , cdTypeParams :: [TypeParam a]        -- ^ Type parameters.
  , cdSuperClass :: Maybe (ClassType a)  -- ^ Super class if the class has one.
  , cdInterfaces :: [ClassType a]        -- ^ Interfaces it implements.
  , cdBody       :: ClassBody a          -- ^ Class body.
  } deriving (Show, Eq)

-- | Interface declaration.
data InterfaceDecl a = InterfaceDecl
  { intdAnn        :: a                -- ^ Annotation.
  , intdModifiers  :: [Modifier a]     -- ^ Modifiers.
  , intdId         :: Id a             -- ^ Interface identifier.
  , intdTypeParams :: [TypeParam a]    -- ^ Type parameters.
  , intdInterfaces :: [ClassType a]    -- ^ Interfaces it extends.
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
  | Typemethod a  -- ^ typemethod
  | Reflexive  a  -- ^ reflexive
  | Transitive a  -- ^ transitive
  | Symmetric  a  -- ^ symmetric
  | Readonly   a  -- ^ readonly
  | Notnull    a  -- ^ notnull
  -- TODO: more Paragon modifiers
  deriving (Show, Eq, Functor)

data TypeParam a = TP
  deriving (Show, Eq)

data ClassType a = CT
  deriving (Show, Eq)

-- | Class body.
data ClassBody a = ClassBody
  { cbAnn   :: a                  -- ^ Annotation.
  , cbDecls :: [ClassBodyDecl a]  -- ^ Declarations.
  } deriving (Show, Eq)

data InterfaceBody a = IB
  deriving (Show, Eq)

-- | Declaration in class body.
data ClassBodyDecl a =
    -- | Member declaration.
    MemberDecl { clBodyDeclAnn        :: a             -- ^ Annotation.
               , clBodyDeclMemberDecl :: MemberDecl a  -- ^ Member declaration.
               }
  -- TODO: InitDecl
  deriving (Show, Eq)

-- | Member declaration.
data MemberDecl a =
    -- | Field declaration.
    FieldDecl { membDeclAnn        :: a             -- ^ Annotation.
              , fieldDeclModifiers :: [Modifier a]  -- ^ Modifiers.
              , fieldDeclType      :: Type a        -- ^ Field type.
              , fieldDeclVarDecls  :: [VarDecl a]   -- ^ Variable declarators.
              }
    -- | Method declaration.
  | MethodDecl { membDeclAnn            :: a                -- ^ Annotation.
               , methodDeclModifiers    :: [Modifier a]     -- ^ Modifiers.
               , methodDeclTypeParams   :: [TypeParam a]    -- ^ Type parameters of generic method.
               , methodDeclReturnType   :: ReturnType a     -- ^ Method return type.
               , methodDeclId           :: Id a             -- ^ Method identifier.
               , methodDeclFormalParams :: [FormalParam a]  -- ^ Formal parameters.
               -- TODO: exceptions
               , methodDeclBody         :: MethodBody a     -- ^ Method body.
               }
  deriving (Show, Eq)

-- | Variable declaration with optional initializer.
data VarDecl a = VarDecl
  { varDeclAnn  :: a                  -- ^ Annotation.
  , varDeclId   :: Id a               -- ^ Variable identifier.
  } deriving (Show, Eq)

-- | Method formal parameter.
data FormalParam a = FormalParam
  { formalParamAnn       :: a             -- ^ Annotation.
  , formalParamModifiers :: [Modifier a]  -- ^ Modifiers.
  , formalParamType      :: Type a        -- ^ Parameter type.
  , formalParamVarArity  :: Bool          -- ^ Is it varargs parameter (variable arity).
  , formalParamId        :: Id a          -- ^ Parameter identifier.
  } deriving (Show, Eq)

-- | Method body or the lack of it.
data MethodBody a = MethodBody
  { methodBodyAnn   :: a                -- ^ Annotation.
  , methodBodyBlock :: Maybe (Block a)  -- ^ Optional method body (code block) or semicolon.
  } deriving (Show, Eq)

-- | Code block.
data Block a = Block
  { blockAnn      :: a              -- ^ Annotation.
  , blockAnnStmts :: [BlockStmt a]  -- ^ Block statements.
  } deriving (Show, Eq)

-- | Block statement.
data BlockStmt a =
    -- | Normal statement.
    BlockStmt { blockStmtAnn :: a       -- ^ Annotation.
              , blockStmt    :: Stmt a  -- ^ Statement.
              }
    -- | Local variable declaration.
  | LocalVars { blockStmtAnn       :: a             -- ^ Annotation.
              , localVarsModifiers :: [Modifier a]  -- ^ Modifiers.
              , localVarsType      :: Type a        -- ^ Variable declaration type.
              , localVarsDecls     :: [VarDecl a]   -- ^ Variable declarators.
              }
  deriving (Show, Eq)

-- | Statement.
data Stmt a = S
  deriving (Show, Eq)

-- Types

-- | Top-level data type for Paragon types.
data Type a =
    -- | Primitive type.
    PrimType { typeAnn      :: a           -- ^ Annotation.
             , typePrimType :: PrimType a  -- ^ Primitive type.
             }
    -- | Reference type.
  | RefType { typeAnn     :: a          -- ^ Annotation.
            , typeRefType :: RefType a  -- ^ Reference type.
            }
  deriving (Show, Eq)

-- | Representation of possible return types.
data ReturnType a =
    -- | void.
    VoidType { retTypeAnn :: a }
    -- | Lock type.
  | LockType { retTypeAnn :: a }
    -- | Other types.
  | Type { retTypeAnn :: a
         , retType    :: Type a
         }
  deriving (Show, Eq)

-- | Primitive types.
data PrimType a =
    BooleanT a
  | ByteT    a
  | ShortT   a
  | IntT     a
  | LongT    a
  | CharT    a
  | FloatT   a
  | DoubleT  a
  -- Paragon specific
  | ActorT   a
  | PolicyT  a
  deriving (Show, Eq, Functor)

-- | Reference type.
data RefType a =
    -- | Class type.
    ClassRefType { refTypeAnn       :: a            -- ^ Annotation.
                 , refTypeClassType :: ClassType a  -- ^ Class type.
                 }
    -- | Type variable.
  | TypeVar { refTypeAnn   :: a     -- ^ Annotation.
            , refTypeVarId :: Id a  -- ^ Type variable identifier.
            }
  -- TODO: ArrayType
  deriving (Show, Eq)

$(deriveAnnotatedMany [''Modifier, ''PrimType])

-- Name type helpers.

-- | Creates a qualified name from name type and list of identifiers.
-- Takes a function to combine annotations.
mkName :: (a -> a -> a) -> NameType -> [Id a] -> Name a
mkName combine nameT ids = mkName' (reverse ids)
  where mkName' [i]    = Name (idAnn i) i nameT Nothing
        mkName' (i:is) = let pre = mkName' is
                         in Name (combine (nameAnn pre) (idAnn i)) i nameT (Just pre)
        mkName' [] = panic (syntaxModule ++ ".mkName") "empty list of identifiers"

-- Create qualified names of different name types from list of identifiers.

pkgName :: [Id SrcSpan] -> Name SrcSpan
pkgName = mkName combineSrcSpan PkgName

ambigName :: [Id SrcSpan] -> Name SrcSpan
ambigName = mkName combineSrcSpan AmbigName

