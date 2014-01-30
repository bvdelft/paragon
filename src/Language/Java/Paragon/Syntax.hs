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
  } deriving (Show, Eq, Functor)

-- | Qualified name. A period-separated list of identifiers.
data Name a = Name
  { nameAnn    :: a               -- ^ Annotation.
  , nameId     :: Id a            -- ^ Identifier.
  , nameType   :: NameType        -- ^ Type of the name.
  , namePrefix :: Maybe (Name a)  -- ^ Possibly, name part before the period.
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
  deriving (Show, Eq)

-- | Compilation unit.
data CompilationUnit a = CompilationUnit
  { cuAnn         :: a                      -- ^ Annotation.
  , cuPkgDecl     :: Maybe (PackageDecl a)  -- ^ Package declaration.
  , cuImportDecls :: [ImportDecl a]         -- ^ Import declarations.
  , cuTypeDecls   :: [TypeDecl a]           -- ^ Type declarations.
  } deriving (Show, Eq, Functor)

-- | Package declaration.
data PackageDecl a = PackageDecl
  { pdAnn  :: a       -- ^ Annotation.
  , pdName :: Name a  -- ^ Package name.
  } deriving (Show, Eq, Functor)

-- | Import declaration.
data ImportDecl a =
    -- | Import a single type.
    -- Example: import java.util.LinkedList;
    SingleTypeImport { impdAnn  :: a       -- ^ Annotation.
                     , impdName :: Name a  -- ^ Type\/package\/member name.
                     }

    -- | Import all the types contained in a package.
    -- Example: import java.util.*;
  | TypeImportOnDemand { impdAnn  :: a
                       , impdName :: Name a
                       }

    -- | Static import of a single type.
    -- Example: import static java.lang.Math.PI;
  | SingleStaticImport { impdAnn  :: a
                       , impdName :: Name a
                       }

    -- | Static import of all members.
    -- Example: import static java.lang.Math.*;
  | StaticImportOnDemand { impdAnn  :: a
                         , impdName :: Name a
                         }
  deriving (Show, Eq, Functor)

-- | Class or interface declaration.
data TypeDecl a =
    ClassTypeDecl     { tdAnn       :: a            -- ^ Annotation.
                      , tdClassDecl :: ClassDecl a  -- ^ Class declaration.
                      }
  | InterfaceTypeDecl { tdAnn     :: a
                      , tdIntDecl :: InterfaceDecl a  -- ^ Interface declaration.
                      }
  deriving (Show, Eq, Functor)

-- | Class declaration.
data ClassDecl a = ClassDecl
  { cdAnn        :: a                    -- ^ Annotation.
  , cdModifiers  :: [Modifier a]         -- ^ Modifiers.
  , cdId         :: Id a                 -- ^ Class identifier.
  , cdTypeParams :: [TypeParam a]        -- ^ Type parameters.
  , cdSuperClass :: Maybe (ClassType a)  -- ^ Super class if the class has one.
  , cdInterfaces :: [ClassType a]        -- ^ Interfaces it implements.
  , cdBody       :: ClassBody a          -- ^ Class body.
  } deriving (Show, Eq, Functor)

-- | Interface declaration.
data InterfaceDecl a = InterfaceDecl
  { intdAnn        :: a                -- ^ Annotation.
  , intdModifiers  :: [Modifier a]     -- ^ Modifiers.
  , intdId         :: Id a             -- ^ Interface identifier.
  , intdTypeParams :: [TypeParam a]    -- ^ Type parameters.
  , intdInterfaces :: [ClassType a]    -- ^ Interfaces it extends.
  , intdBody       :: InterfaceBody a  -- ^ Interface body.
  } deriving (Show, Eq, Functor)

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
  deriving (Show, Eq, Functor)

-- | Class body.
data ClassBody a = ClassBody
  { cbAnn   :: a                  -- ^ Annotation.
  , cbDecls :: [ClassBodyDecl a]  -- ^ Declarations.
  } deriving (Show, Eq, Functor)

data InterfaceBody a = IB
  deriving (Show, Eq, Functor)

-- | Declaration in class body.
data ClassBodyDecl a =
    -- | Member declaration.
    MemberDecl { clBodyDeclAnn        :: a             -- ^ Annotation.
               , clBodyDeclMemberDecl :: MemberDecl a  -- ^ Member declaration.
               }
  -- TODO: InitDecl
  deriving (Show, Eq, Functor)

-- | Member declaration.
data MemberDecl a =
    -- | Field declaration.
    FieldDecl { membDeclAnn        :: a             -- ^ Annotation.
              , fieldDeclModifiers :: [Modifier a]  -- ^ Modifiers.
              , fieldDeclType      :: Type a        -- ^ Field type.
              , fieldDeclVarDecls  :: [VarDecl a]   -- ^ Variable declarators.
              }
    -- | Method declaration.
  | MethodDecl { membDeclAnn            :: a
               , methodDeclModifiers    :: [Modifier a]     -- ^ Modifiers.
               , methodDeclTypeParams   :: [TypeParam a]    -- ^ Type parameters of generic method.
               , methodDeclReturnType   :: ReturnType a     -- ^ Method return type.
               , methodDeclId           :: Id a             -- ^ Method identifier.
               , methodDeclFormalParams :: [FormalParam a]  -- ^ Formal parameters.
               -- TODO: exceptions
               , methodDeclBody         :: MethodBody a     -- ^ Method body.
               }
  deriving (Show, Eq, Functor)

-- | Variable declaration with optional initializer.
data VarDecl a = VarDecl
  { varDeclAnn  :: a                  -- ^ Annotation.
  , varDeclId   :: Id a               -- ^ Variable identifier.
  } deriving (Show, Eq, Functor)

-- | Method formal parameter.
data FormalParam a = FormalParam
  { formalParamAnn       :: a             -- ^ Annotation.
  , formalParamModifiers :: [Modifier a]  -- ^ Modifiers.
  , formalParamType      :: Type a        -- ^ Parameter type.
  , formalParamVarArity  :: Bool          -- ^ Is it varargs parameter (variable arity).
  , formalParamId        :: Id a          -- ^ Parameter identifier.
  } deriving (Show, Eq, Functor)

-- | Method body or the lack of it.
data MethodBody a = MethodBody
  { methodBodyAnn   :: a                -- ^ Annotation.
  , methodBodyBlock :: Maybe (Block a)  -- ^ Optional method body (code block) or semicolon.
  } deriving (Show, Eq, Functor)

-- | Code block.
data Block a = Block
  { blockAnn      :: a              -- ^ Annotation.
  , blockAnnStmts :: [BlockStmt a]  -- ^ Block statements.
  } deriving (Show, Eq, Functor)

-- | Block statement.
data BlockStmt a =
    -- | Normal statement.
    BlockStmt { blockStmtAnn  :: a       -- ^ Annotation.
              , blockStmtStmt :: Stmt a  -- ^ Statement.
              }
    -- | Local variable declaration.
  | LocalVars { blockStmtAnn       :: a
              , localVarsModifiers :: [Modifier a]  -- ^ Modifiers.
              , localVarsType      :: Type a        -- ^ Variable declaration type.
              , localVarsDecls     :: [VarDecl a]   -- ^ Variable declarators.
              }
  deriving (Show, Eq, Functor)

-- | Statements.
data Stmt a =
    -- | Empty statement - semicolon.
    Empty { stmtAnn :: a -- ^ Annotation.
          }
    -- | Expression statement (e.g. assignment, incrementation, decrementation,
    -- method invocation etc.).
  | ExpStmt { stmtAnn :: a
            , stmtExp :: Exp a  -- ^ Expression.
            }
  deriving (Show, Eq, Functor)

-- | Expressions.
data Exp a =
    -- | Literal.
    Lit { expAnn :: a          -- ^ Annotation.
        , expLit :: Literal a  -- ^ Literal.
        }
    -- | Referencing some name, e.g. variable.
  | NameExp { expAnn      :: a
            , nameExpName :: Name a  -- ^ Name of a variable, for example.
            }
    -- | Assignment.
  | Assign { expAnn    :: a
           , assignLhs :: Lhs a       -- ^ Left-hand side of the assignment.
           , assignOp  :: AssignOp a  -- ^ Assignment operator (=, +=, *=, ...).
           , assignExp :: Exp a       -- ^ Expression on the right-hand side.
           }
  deriving (Show, Eq, Functor)

-- | Types of literals.
data Literal a =
    Int { litAnn    :: a        -- ^ Annotation.
        , intLitVal :: Integer  -- ^ Value of integer literal.
        }
  | Long { litAnn     :: a
         , longLitVal :: Integer  -- ^ Value of long literal.
         }
  | Double { litAnn       :: a
           , doubleLitVal :: Double  -- ^ Value of double literal.
           }
  | Float { litAnn      :: a
          , floatLitVal :: Double  -- ^ Value of float literal.
          }
  | Char { litAnn     :: a
         , charLitVal :: Char  -- ^ Value of char literal.
         }
  | String { litAnn       :: a
           , stringLitVal :: String  -- ^ Value of string literal.
           }
  | Boolean { litAnn     :: a
            , boolLitVal :: Bool  -- ^ Value of boolean literal.
            }
  | Null { litAnn :: a }
  deriving (Show, Eq, Functor)

-- | Left-hand side of an assignment expression.
data Lhs a =
  -- | Variable.
  NameLhs { lhsAnn  :: a       -- ^ Annotation.
          , lhsName :: Name a  -- ^ Variable name.
          }
  deriving (Show, Eq, Functor)

-- | Different assignment operators.
data AssignOp a =
  EqualA a  -- ^ =
  deriving (Show, Eq, Functor)

-- Types

-- | Top-level data type for Paragon types.
data Type a =
    -- | Primitive type.
    PrimType { typeAnn      :: a           -- ^ Annotation.
             , typePrimType :: PrimType a  -- ^ Primitive type.
             }
    -- | Reference type.
  | RefType { typeAnn     :: a
            , typeRefType :: RefType a  -- ^ Reference type.
            }
  deriving (Show, Eq, Functor)

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
  | PolicyT  a
  deriving (Show, Eq, Functor)

-- | Reference type.
data RefType a =
    -- | Class type.
    ClassRefType { refTypeAnn       :: a            -- ^ Annotation.
                 , refTypeClassType :: ClassType a  -- ^ Class type.
                 }
  -- TODO: ArrayType
  deriving (Show, Eq, Functor)

-- | Class or interface type.
data ClassType a = ClassType a (Name a) [TypeArgument a]
  deriving (Show, Eq, Functor)

-- | Representation of type arguments of generic types.
data TypeArgument a = TA
  deriving (Show, Eq, Functor)

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
  deriving (Show, Eq, Functor)

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

pkgOrTypeName :: [Id SrcSpan] -> Name SrcSpan
pkgOrTypeName = mkNameSrcSpan PkgOrTypeName

expOrLockName :: [Id SrcSpan] -> Name SrcSpan
expOrLockName = mkNameSrcSpan ExpOrLockName

ambigName :: [Id SrcSpan] -> Name SrcSpan
ambigName = mkNameSrcSpan AmbigName

