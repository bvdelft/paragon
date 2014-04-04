{-# LANGUAGE DeriveDataTypeable #-}
-- | Paragon Abstract Syntax Tree. Expressions.
module Language.Java.Paragon.Syntax.Expressions
  (
    module Language.Java.Paragon.Syntax.Expressions
  , module Language.Java.Paragon.Syntax.Names
  , module Language.Java.Paragon.Syntax.Types
  ) where

import Data.Data

import Language.Java.Paragon.Syntax.Names
import Language.Java.Paragon.Syntax.Types

import Language.Java.Paragon.Annotation
import Language.Java.Paragon.Annotated

-- | Expressions. Unsafe records.
data Exp =
    -- | Literal.
    Lit Literal
    -- | Referencing some name, e.g. variable.
  | NameExp Name
    -- | Assignment.
  | Assign { assignAnn :: Annotation  -- ^ Annotation.
           , assignLhs :: Lhs         -- ^ Left-hand side of the assignment.
           , assignOp  :: AssignOp    -- ^ Assignment operator (=, +=, *=, ...).
           , assignExp :: Exp         -- ^ Expression on the right-hand side.
           }
    -- | Policy expression.
  | PolicyExp PolicyExp
  deriving (Data, Typeable, Ord, Show, Eq)

-- | Types of literals. Unsafe records.
data Literal =
    Int { litAnn    :: Annotation  -- ^ Annotation.
        , intLitVal :: Integer     -- ^ Value of integer literal.
        }
  | Long { litAnn     :: Annotation  -- ^ Annotation.
         , longLitVal :: Integer     -- ^ Value of long literal.
         }
  | Double { litAnn       :: Annotation  -- ^ Annotation.
           , doubleLitVal :: Double      -- ^ Value of double literal.
           }
  | Float { litAnn      :: Annotation  -- ^ Annotation.
          , floatLitVal :: Double      -- ^ Value of float literal.
          }
  | Char { litAnn     :: Annotation  -- ^ Annotation.
         , charLitVal :: Char        -- ^ Value of char literal.
         }
  | String { litAnn       :: Annotation  -- ^ Annotation.
           , stringLitVal :: String      -- ^ Value of string literal.
           }
  | Boolean { litAnn     :: Annotation  -- ^ Annotation.
            , boolLitVal :: Bool        -- ^ Value of boolean literal.
            }
  | Null { litAnn :: Annotation }
  deriving (Data, Typeable, Ord, Show, Eq)
  
-- | Left-hand side of an assignment expression.
data Lhs =
  -- | Variable.
  NameLhs { lhsName :: Name }
  deriving (Data, Typeable, Ord, Show, Eq)

-- | Different assignment operators.
data AssignOp =
  EqualA Annotation  -- ^ =
  deriving (Data, Typeable, Ord, Show, Eq)

-- | A policy is a conjunction (set) of clauses, represented as a list.
data PolicyExp =
    -- | Policy literal.
    PolicyLit { policyAnn     :: Annotation  -- ^ Annotation.
              , policyClauses :: [Clause]    -- ^ Set of clauses.
              }
  deriving (Data, Typeable, Ord, Show, Eq)

-- | A clause of the form Sigma => a, where a is an actor and Sigma a set of 
-- locks/atomic predicates that must be open/true.
data Clause = Clause
  { clauseAnn      :: Annotation       -- ^ Annotation.
  , clauseVarDecls :: [ClauseVarDecl]  -- ^ Clause variable declarations.
  , clauseHead     :: ClauseHead       -- ^ Head of the clause.
  , clauseAtoms    :: [Atom]           -- ^ Clause atoms.
  } deriving (Data, Typeable, Ord, Show, Eq)

-- | Clause variable declaration.
data ClauseVarDecl = ClauseVarDecl
  { clauseVarDeclAnn  :: Annotation  -- ^ Annotation.
  , clauseVarDeclType :: RefType     -- ^ Type of clause variable.
  , clauseVarDeclId   :: Id          -- ^ Variable identifier.
  } deriving (Data, Typeable, Ord, Show, Eq)

-- | Head of the clause.
data ClauseHead =
    ClauseDeclHead ClauseVarDecl
  | ClauseVarHead Actor
  deriving (Data, Typeable, Ord, Show, Eq)

-- | Actor variable.
data Actor =
    -- | Free actor variable (and thus concrete w.r.t. the policy under scrutiny).
    Actor ActorName
    -- | Forall quantified actor variable within the current clause.
  | Var Id
  deriving (Data, Typeable, Ord, Show, Eq)
  
-- | Representation of actor names. Unsafe records.
data ActorName =
    -- | Free actor variable.
    ActorName Name
    -- | Free actor type parameter.
  | ActorTypeVar { actorTypeVarAnn  :: Annotation  -- ^ Annotation.
                 , actorTypeVarType :: RefType     -- ^ Type of actor type variable.
                 , actorTypeVarId   :: Id          -- ^ Actor type variable identifier.
                 }
  deriving (Data, Typeable, Ord, Show, Eq)

data Atom = Atom
  { atomAnn    :: Annotation  -- ^ Annotation.
  , atomName   :: Name        -- ^ Atom name.
  , atomActors :: [Actor]     -- ^ Atom actors.
  } deriving (Data, Typeable, Ord, Show, Eq)
 
-- | Policy representation.
type Policy = Exp

instance Annotated Exp where
  getAnn (Lit       x) = getAnn x
  getAnn (NameExp   x) = getAnn x
  getAnn a@(Assign {}) = assignAnn a
  getAnn (PolicyExp x) = getAnn x
  setAnn a (Lit       x) = Lit       $ setAnn a x
  setAnn a (NameExp   x) = NameExp   $ setAnn a x
  setAnn a x@(Assign {}) = x { assignAnn = a }
  setAnn a (PolicyExp x) = PolicyExp $ setAnn a x

instance Annotated Literal where
  getAnn = litAnn
  setAnn a x = x {litAnn = a}

instance Annotated Lhs where
  getAnn = getAnn . lhsName
  setAnn a x = x { lhsName = setAnn a (lhsName x) }

instance Annotated AssignOp where
  getAnn (EqualA x) = x
  setAnn a (EqualA _) = EqualA a

instance Annotated PolicyExp where
  getAnn p@(PolicyLit {}) = policyAnn p
  setAnn a x@(PolicyLit {}) = x { policyAnn = a }

instance Annotated Clause where
  getAnn = clauseAnn
  setAnn a x = x {clauseAnn = a}

instance Annotated ClauseVarDecl where
  getAnn = clauseVarDeclAnn
  setAnn a x = x {clauseVarDeclAnn = a}

instance Annotated ClauseHead where
  getAnn (ClauseDeclHead x) = getAnn x
  getAnn (ClauseVarHead  x) = getAnn x
  setAnn a (ClauseDeclHead x) = ClauseDeclHead $ setAnn a x
  setAnn a (ClauseVarHead  x) = ClauseVarHead  $ setAnn a x

instance Annotated Actor where
  getAnn (Actor x) = getAnn x
  getAnn (Var   x) = getAnn x
  setAnn a (Actor x) = Actor $ setAnn a x
  setAnn a (Var   x) = Var   $ setAnn a x

instance Annotated ActorName where
  getAnn (ActorName x) = getAnn x
  getAnn a@(ActorTypeVar {}) = actorTypeVarAnn a
  setAnn a (ActorName x) = ActorName $ setAnn a x
  setAnn a x@(ActorTypeVar {}) = x { actorTypeVarAnn = a }

instance Annotated Atom where
  getAnn = atomAnn
  setAnn a x = x {atomAnn = a}
