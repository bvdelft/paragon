{-# LANGUAGE TemplateHaskell
           , DeriveFunctor
 #-}

-- | Paragon Abstract Syntax Tree. Expressions.
module Language.Java.Paragon.Syntax.Expressions
  (
    module Language.Java.Paragon.Syntax.Expressions
  , module Language.Java.Paragon.Syntax.Names
  , module Language.Java.Paragon.Syntax.Types
  ) where

import Language.Java.Paragon.Syntax.Names
import Language.Java.Paragon.Syntax.Types

import Language.Java.Paragon.Annotated

-- | Expressions. Unsafe records.
data Exp a =
    -- | Literal.
    Lit (Literal a)
    -- | Referencing some name, e.g. variable.
  | NameExp (Name a)
    -- | Assignment.
  | Assign { assignAnn :: a           -- ^ Annotation.
           , assignLhs :: Lhs a       -- ^ Left-hand side of the assignment.
           , assignOp  :: AssignOp a  -- ^ Assignment operator (=, +=, *=, ...).
           , assignExp :: Exp a       -- ^ Expression on the right-hand side.
           }
    -- | Policy expression.
  | PolicyExp (PolicyExp a)
  deriving (Show, Eq, Functor)

-- | Types of literals. Unsafe records.
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
  NameLhs { lhsName :: Name a }
  deriving (Show, Eq, Functor)

-- | Different assignment operators.
data AssignOp a =
  EqualA a  -- ^ =
  deriving (Show, Eq, Functor)

-- | A policy is a conjunction (set) of clauses, represented as a list.
data PolicyExp a =
    -- | Policy literal.
    PolicyLit { policyAnn     :: a           -- ^ Annotation.
              , policyClauses :: [Clause a]  -- ^ Set of clauses.
              }
  deriving (Show, Eq, Functor)

-- | A clause of the form Sigma => a, where a is an actor and Sigma a set of 
-- locks/atomic predicates that must be open/true.
data Clause a = Clause
  { clauseAnn      :: a                  -- ^ Annotation.
  , clauseVarDecls :: [ClauseVarDecl a]  -- ^ Clause variable declarations.
  , clauseHead     :: ClauseHead a       -- ^ Head of the clause.
  , clauseAtoms    :: [Atom a]           -- ^ Clause atoms.
  } deriving (Show, Eq, Functor)

-- | Clause variable declaration.
data ClauseVarDecl a = ClauseVarDecl
  { clauseVarDeclAnn  :: a          -- ^ Annotation.
  , clauseVarDeclType :: RefType a  -- ^ Type of clause variable.
  , clauseVarDeclId   :: Id a       -- ^ Variable identifier.
  } deriving (Show, Eq, Functor)

-- | Head of the clause.
data ClauseHead a =
    ClauseDeclHead (ClauseVarDecl a)
  | ClauseVarHead (Actor a)
  deriving (Show, Eq, Functor)

-- | Actor variable.
data Actor a =
    -- | Free actor variable (and thus concrete w.r.t. the policy under scrutiny).
    Actor (ActorName a)
    -- | Forall quantified actor variable within the current clause.
  | Var (Id a)
  deriving (Show, Eq, Functor)

-- | Representation of actor names. Unsafe records.
data ActorName a =
    -- | Free actor variable.
    ActorName (Name a)
    -- | Free actor type parameter.
  | ActorTypeVar { actorTypeVarAnn  :: a          -- ^ Annotation.
                 , actorTypeVarType :: RefType a  -- ^ Type of actor type variable.
                 , actorTypeVarId   :: Id a       -- ^ Actor type variable identifier.
                 }
  deriving (Show, Eq, Functor)

data Atom a = Atom
  { atomAnn    :: a          -- ^ Annotation.
  , atomName   :: Name a     -- ^ Atom name.
  , atomActors :: [Actor a]  -- ^ Atom actors.
  } deriving (Show, Eq, Functor)

-- | Policy representation.
type Policy a = Exp a

$(deriveAnnotatedMany
  [ ''Exp
  , ''Literal
  , ''Lhs
  , ''AssignOp
  , ''PolicyExp
  , ''Clause
  , ''ClauseVarDecl
  , ''ClauseHead
  , ''Actor
  , ''ActorName
  , ''Atom
  ])

