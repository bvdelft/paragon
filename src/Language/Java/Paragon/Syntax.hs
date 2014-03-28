-- | Paragon Abstract Syntax Tree.
module Language.Java.Paragon.Syntax
  (
    module Language.Java.Paragon.Syntax
  , module Language.Java.Paragon.Syntax.Names
  , module Language.Java.Paragon.Syntax.Types
  , module Language.Java.Paragon.Syntax.Statements
  , module Language.Java.Paragon.Syntax.Expressions
  , module Language.Java.Paragon.Syntax.Modifiers
  , module Language.Java.Paragon.Annotated
  ) where

import Language.Java.Paragon.Interaction (libraryBase)

import Language.Java.Paragon.Syntax.Names
import Language.Java.Paragon.Syntax.Types
import Language.Java.Paragon.Syntax.Statements
import Language.Java.Paragon.Syntax.Expressions
import Language.Java.Paragon.Syntax.Modifiers

import Language.Java.Paragon.Annotated
import Language.Java.Paragon.Annotation

syntaxModule :: String
syntaxModule = libraryBase ++ ".Syntax"

-- | Type synonym for top-level AST node.
type AST = CompilationUnit

-- | Compilation unit.
data CompilationUnit = CompilationUnit
  { cuAnn         :: Annotation         -- ^ Annotation.
  , cuPkgDecl     :: Maybe PackageDecl  -- ^ Package declaration.
  , cuImportDecls :: [ImportDecl]       -- ^ Import declarations.
  , cuTypeDecls   :: [TypeDecl]         -- ^ Type declarations.
  } deriving (Show, Eq)

-- | Package declaration.
data PackageDecl = PackageDecl
  { pdAnn  :: Annotation  -- ^ Annotation.
  , pdName :: Name        -- ^ Package name.
  } deriving (Show, Eq)

-- | Import declaration.
data ImportDecl =
    -- | Import a single type.
    -- Example: import java.util.LinkedList;
    SingleTypeImport { impdAnn  :: Annotation  -- ^ Annotation.
                     , impdName :: Name        -- ^ Type\/package\/member name.
                     }

    -- | Import all the types contained in a package.
    -- Example: import java.util.*;
  | TypeImportOnDemand { impdAnn  :: Annotation
                       , impdName :: Name
                       }

    -- | Static import of a single type.
    -- Example: import static java.lang.Math.PI;
  | SingleStaticImport { impdAnn  :: Annotation
                       , impdName :: Name
                       }

    -- | Static import of all members.
    -- Example: import static java.lang.Math.*;
  | StaticImportOnDemand { impdAnn  :: Annotation
                         , impdName :: Name
                         }
  deriving (Show, Eq)

-- | Class or interface declaration.
data TypeDecl =
    -- | Class declaration.
    ClassTypeDecl ClassDecl
    -- | Interface declaration.
  | InterfaceTypeDecl InterfaceDecl
  deriving (Show, Eq)

-- | Class declaration.
data ClassDecl = ClassDecl
  { cdAnn        :: Annotation       -- ^ Annotation.
  , cdModifiers  :: [Modifier]       -- ^ Modifiers.
  , cdId         :: Id               -- ^ Class identifier.
  , cdTypeParams :: [TypeParam]      -- ^ Type parameters.
  , cdSuperClass :: Maybe ClassType  -- ^ Super class if the class has one.
  , cdInterfaces :: [ClassType]      -- ^ Interfaces it implements.
  , cdBody       :: ClassBody        -- ^ Class body.
  } deriving (Show, Eq)

-- | Interface declaration.
data InterfaceDecl = InterfaceDecl
  { intdAnn        :: Annotation     -- ^ Annotation.
  , intdModifiers  :: [Modifier]     -- ^ Modifiers.
  , intdId         :: Id             -- ^ Interface identifier.
  , intdTypeParams :: [TypeParam]    -- ^ Type parameters.
  , intdInterfaces :: [ClassType]    -- ^ Interfaces it extends.
  , intdBody       :: InterfaceBody  -- ^ Interface body.
  } deriving (Show, Eq)

data TypeParam = TP
  deriving (Show, Eq)

-- | Class body.
data ClassBody = ClassBody
  { cbAnn   :: Annotation       -- ^ Annotation.
  , cbDecls :: [ClassBodyDecl]  -- ^ Declarations.
  } deriving (Show, Eq)

data InterfaceBody = IB
  deriving (Show, Eq)

-- | Declaration in class body.
data ClassBodyDecl =
    -- | Member declaration.
    MemberDecl MemberDecl
  -- TODO: InitDecl
  deriving (Show, Eq)

-- | Member declaration. Unsafe records.
data MemberDecl =
    -- | Field declaration.
    FieldDecl { membDeclAnn        :: Annotation  -- ^ Annotation.
              , fieldDeclModifiers :: [Modifier]  -- ^ Modifiers.
              , fieldDeclType      :: Type        -- ^ Field type.
              , fieldDeclVarDecls  :: [VarDecl]   -- ^ Variable declarators.
              }
    -- | Method declaration.
  | MethodDecl { membDeclAnn            :: Annotation     -- ^ Annotation.
               , methodDeclModifiers    :: [Modifier]     -- ^ Modifiers.
               , methodDeclTypeParams   :: [TypeParam]    -- ^ Type parameters of generic method.
               , methodDeclReturnType   :: ReturnType     -- ^ Method return type.
               , methodDeclId           :: Id             -- ^ Method identifier.
               , methodDeclFormalParams :: [FormalParam]  -- ^ Formal parameters.
               -- TODO: exceptions
               , methodDeclBody         :: MethodBody     -- ^ Method body.
               }
  deriving (Show, Eq)

-- | Variable/field declaration with optional initializer.
data VarDecl = VarDecl
  { varDeclAnn  :: Annotation     -- ^ Annotation.
  , varDeclId   :: Id             -- ^ Variable identifier.
  , varDeclInit :: Maybe VarInit  -- ^ Optional variable initializer.
  } deriving (Show, Eq)

-- | Method formal parameter.
data FormalParam = FormalParam
  { formalParamAnn       :: Annotation  -- ^ Annotation.
  , formalParamModifiers :: [Modifier]  -- ^ Modifiers.
  , formalParamType      :: Type        -- ^ Parameter type.
  , formalParamVarArity  :: Bool        -- ^ Is it varargs parameter (variable arity).
  , formalParamId        :: Id          -- ^ Parameter identifier.
  } deriving (Show, Eq)

-- | Method body or the lack of it.
data MethodBody = MethodBody
  { methodBodyAnn   :: Annotation   -- ^ Annotation.
  , methodBodyBlock :: Maybe Block  -- ^ Optional method body (code block) or semicolon.
  } deriving (Show, Eq)

-- | Explicit initializer for field/variable declaration.
data VarInit = InitExp { varInitExp :: Exp }
  deriving (Show, Eq)

-- | Code block.
data Block = Block
  { blockAnn      :: Annotation   -- ^ Annotation.
  , blockAnnStmts :: [BlockStmt]  -- ^ Block statements.
  } deriving (Show, Eq)

-- | Block statement. Unsafe records.
data BlockStmt =
    -- | Normal statement.
    BlockStmt Stmt
    -- | Local variable declaration.
  | LocalVars { localVarsAnn       :: Annotation  -- ^ Annotation.
              , localVarsModifiers :: [Modifier]  -- ^ Modifiers.
              , localVarsType      :: Type        -- ^ Variable declaration type.
              , localVarsDecls     :: [VarDecl]   -- ^ Variable declarators.
              }
  deriving (Show, Eq)

instance Annotated CompilationUnit where
  ann = cuAnn

instance Annotated PackageDecl where
  ann = pdAnn

instance Annotated ImportDecl where
  ann = impdAnn

instance Annotated TypeDecl where
  ann (ClassTypeDecl     x) = ann x
  ann (InterfaceTypeDecl x) = ann x

instance Annotated ClassDecl where
  ann = cdAnn

instance Annotated InterfaceDecl where
  ann = intdAnn

instance Annotated ClassBody where
  ann = cbAnn

instance Annotated ClassBodyDecl where
  ann (MemberDecl x) = ann x

instance Annotated MemberDecl where
  ann = membDeclAnn

instance Annotated VarDecl where
  ann = varDeclAnn

instance Annotated FormalParam where
  ann = formalParamAnn

instance Annotated MethodBody where
  ann = methodBodyAnn

instance Annotated VarInit where
  ann = ann . varInitExp

instance Annotated Block where
  ann = blockAnn

instance Annotated BlockStmt where
  ann (BlockStmt x) = ann x
  ann x@(LocalVars {}) = localVarsAnn x
