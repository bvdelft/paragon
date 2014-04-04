{-# LANGUAGE DeriveDataTypeable #-}
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

import Data.Data

import Language.Java.Paragon.Interaction.Headers (libraryBase)

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
  } deriving (Typeable, Data, Ord, Show, Eq)

-- | Package declaration.
data PackageDecl = PackageDecl
  { pdAnn  :: Annotation  -- ^ Annotation.
  , pdName :: Name        -- ^ Package name.
  } deriving (Typeable, Data, Ord, Show, Eq)

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
  deriving (Typeable, Data, Ord, Show, Eq)

-- | Class or interface declaration.
data TypeDecl =
    -- | Class declaration.
    ClassTypeDecl ClassDecl
    -- | Interface declaration.
  | InterfaceTypeDecl InterfaceDecl
  deriving (Typeable, Data, Ord, Show, Eq)

-- | Class declaration.
data ClassDecl = ClassDecl
  { cdAnn        :: Annotation       -- ^ Annotation.
  , cdModifiers  :: [Modifier]       -- ^ Modifiers.
  , cdId         :: Id               -- ^ Class identifier.
  , cdTypeParams :: [TypeParam]      -- ^ Type parameters.
  , cdSuperClass :: Maybe ClassType  -- ^ Super class if the class has one.
  , cdInterfaces :: [ClassType]      -- ^ Interfaces it implements.
  , cdBody       :: ClassBody        -- ^ Class body.
  } deriving (Typeable, Data, Ord, Show, Eq)

-- | Interface declaration.
data InterfaceDecl = InterfaceDecl
  { intdAnn        :: Annotation     -- ^ Annotation.
  , intdModifiers  :: [Modifier]     -- ^ Modifiers.
  , intdId         :: Id             -- ^ Interface identifier.
  , intdTypeParams :: [TypeParam]    -- ^ Type parameters.
  , intdInterfaces :: [ClassType]    -- ^ Interfaces it extends.
  , intdBody       :: InterfaceBody  -- ^ Interface body.
  } deriving (Typeable, Data, Ord, Show, Eq)

data TypeParam = TP
  deriving (Data, Typeable, Show, Eq, Ord)

-- | Class body.
data ClassBody = ClassBody
  { cbAnn   :: Annotation       -- ^ Annotation.
  , cbDecls :: [ClassBodyDecl]  -- ^ Declarations.
  } deriving (Typeable, Data, Ord, Show, Eq)

data InterfaceBody = IB
  deriving (Typeable, Data, Ord, Show, Eq)

-- | Declaration in class body.
data ClassBodyDecl =
    -- | Member declaration.
    MemberDecl MemberDecl
  -- TODO: InitDecl
  deriving (Typeable, Data, Ord, Show, Eq)

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
  deriving (Typeable, Data, Ord, Show, Eq)

-- | Variable/field declaration with optional initializer.
data VarDecl = VarDecl
  { varDeclAnn  :: Annotation     -- ^ Annotation.
  , varDeclId   :: Id             -- ^ Variable identifier.
  , varDeclInit :: Maybe VarInit  -- ^ Optional variable initializer.
  } deriving (Typeable, Data, Ord, Show, Eq)

-- | Method formal parameter.
data FormalParam = FormalParam
  { formalParamAnn       :: Annotation  -- ^ Annotation.
  , formalParamModifiers :: [Modifier]  -- ^ Modifiers.
  , formalParamType      :: Type        -- ^ Parameter type.
  , formalParamVarArity  :: Bool        -- ^ Is it varargs parameter (variable arity).
  , formalParamId        :: Id          -- ^ Parameter identifier.
  } deriving (Typeable, Data, Ord, Show, Eq)

-- | Method body or the lack of it.
data MethodBody = MethodBody
  { methodBodyAnn   :: Annotation   -- ^ Annotation.
  , methodBodyBlock :: Maybe Block  -- ^ Optional method body (code block) or semicolon.
  } deriving (Typeable, Data, Ord, Show, Eq)

-- | Explicit initializer for field/variable declaration.
data VarInit = InitExp { varInitExp :: Exp }
  deriving (Typeable, Data, Ord, Show, Eq)

-- | Code block.
data Block = Block
  { blockAnn      :: Annotation   -- ^ Annotation.
  , blockAnnStmts :: [BlockStmt]  -- ^ Block statements.
  } deriving (Typeable, Data, Ord, Show, Eq)

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
  deriving (Typeable, Data, Ord, Show, Eq)

instance Annotated CompilationUnit where
  getAnn = cuAnn
  setAnn a x = x {cuAnn = a}

instance Annotated PackageDecl where
  getAnn = pdAnn
  setAnn a x = x {pdAnn = a}

instance Annotated ImportDecl where
  getAnn = impdAnn
  setAnn a x = x {impdAnn = a}

instance Annotated TypeDecl where
  getAnn (ClassTypeDecl     x) = getAnn x
  getAnn (InterfaceTypeDecl x) = getAnn x
  setAnn a (ClassTypeDecl     x) = ClassTypeDecl     $ setAnn a x
  setAnn a (InterfaceTypeDecl x) = InterfaceTypeDecl $ setAnn a x

instance Annotated ClassDecl where
  getAnn = cdAnn
  setAnn a x = x {cdAnn = a}

instance Annotated InterfaceDecl where
  getAnn = intdAnn
  setAnn a x = x {intdAnn = a}

instance Annotated ClassBody where
  getAnn = cbAnn
  setAnn a x = x {cbAnn = a}

instance Annotated ClassBodyDecl where
  getAnn (MemberDecl x) = getAnn x
  setAnn a (MemberDecl x) = MemberDecl $ setAnn a x

instance Annotated MemberDecl where
  getAnn = membDeclAnn
  setAnn a x = x {membDeclAnn = a}

instance Annotated VarDecl where
  getAnn = varDeclAnn
  setAnn a x = x {varDeclAnn = a}

instance Annotated FormalParam where
  getAnn = formalParamAnn
  setAnn a x = x {formalParamAnn = a}

instance Annotated MethodBody where
  getAnn = methodBodyAnn
  setAnn a x = x {methodBodyAnn = a}

instance Annotated VarInit where
  getAnn = getAnn . varInitExp
  setAnn a x = x { varInitExp = setAnn a (varInitExp x) }

instance Annotated Block where
  getAnn = blockAnn
  setAnn a x = x {blockAnn = a}

instance Annotated BlockStmt where
  getAnn (BlockStmt x) = getAnn x
  getAnn x@(LocalVars {}) = localVarsAnn x
  setAnn a (BlockStmt x) = BlockStmt $ setAnn a x
  setAnn a x@(LocalVars {}) = x { localVarsAnn = a }
