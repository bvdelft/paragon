{-# LANGUAGE TemplateHaskell
           , DeriveFunctor
 #-}

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

syntaxModule :: String
syntaxModule = libraryBase ++ ".Syntax"

-- | Type synonym for top-level AST node.
type AST = CompilationUnit

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
    -- | Class declaration.
    ClassTypeDecl (ClassDecl a)
    -- | Interface declaration.
  | InterfaceTypeDecl (InterfaceDecl a)
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
    MemberDecl (MemberDecl a)
  -- TODO: InitDecl
  deriving (Show, Eq, Functor)

-- | Member declaration. Unsafe records.
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

-- | Variable/field declaration with optional initializer.
data VarDecl a = VarDecl
  { varDeclAnn  :: a                  -- ^ Annotation.
  , varDeclId   :: Id a               -- ^ Variable identifier.
  , varDeclInit :: Maybe (VarInit a)  -- ^ Optional variable initializer.
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

-- | Explicit initializer for field/variable declaration.
data VarInit a = InitExp { varInitExp :: Exp a }
  deriving (Show, Eq, Functor)

-- | Code block.
data Block a = Block
  { blockAnn      :: a              -- ^ Annotation.
  , blockAnnStmts :: [BlockStmt a]  -- ^ Block statements.
  } deriving (Show, Eq, Functor)

-- | Block statement. Unsafe records.
data BlockStmt a =
    -- | Normal statement.
    BlockStmt (Stmt a)
    -- | Local variable declaration.
  | LocalVars { localVarsAnn       :: a             -- ^ Annotation.
              , localVarsModifiers :: [Modifier a]  -- ^ Modifiers.
              , localVarsType      :: Type a        -- ^ Variable declaration type.
              , localVarsDecls     :: [VarDecl a]   -- ^ Variable declarators.
              }
  deriving (Show, Eq, Functor)

$(deriveAnnotatedMany
  [ ''CompilationUnit
  , ''PackageDecl
  , ''ImportDecl
  , ''TypeDecl
  , ''ClassDecl
  , ''InterfaceDecl
  , ''ClassBody
  , ''ClassBodyDecl
  , ''MemberDecl
  , ''VarDecl
  , ''FormalParam
  , ''MethodBody
  , ''VarInit
  , ''Block
  , ''BlockStmt
  ])

