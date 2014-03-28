-- | Paragon Abstract Syntax Tree. Expressions.
module Language.Java.Paragon.Syntax.Expressions
  (
    module Language.Java.Paragon.Syntax.Expressions
  , module Language.Java.Paragon.Syntax.Names
  , module Language.Java.Paragon.Syntax.Types
  ) where

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
  deriving (Show, Eq)

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
  deriving (Show, Eq)
  
-- | Left-hand side of an assignment expression.
data Lhs =
  -- | Variable.
  NameLhs { lhsName :: Name }
  deriving (Show, Eq)

-- | Different assignment operators.
data AssignOp =
  EqualA Annotation  -- ^ =
  deriving (Show, Eq)

-- | A policy is a conjunction (set) of clauses, represented as a list.
data PolicyExp =
    -- | Policy literal.
    PolicyLit { policyAnn     :: Annotation  -- ^ Annotation.
              , policyClauses :: [Clause]    -- ^ Set of clauses.
              }
  deriving (Show, Eq)

-- | A clause of the form Sigma => a, where a is an actor and Sigma a set of 
-- locks/atomic predicates that must be open/true.
data Clause = Clause
  { clauseAnn      :: Annotation       -- ^ Annotation.
  , clauseVarDecls :: [ClauseVarDecl]  -- ^ Clause variable declarations.
  , clauseHead     :: ClauseHead       -- ^ Head of the clause.
  , clauseAtoms    :: [Atom]           -- ^ Clause atoms.
  } deriving (Show, Eq)

-- | Clause variable declaration.
data ClauseVarDecl = ClauseVarDecl
  { clauseVarDeclAnn  :: Annotation  -- ^ Annotation.
  , clauseVarDeclType :: RefType     -- ^ Type of clause variable.
  , clauseVarDeclId   :: Id          -- ^ Variable identifier.
  } deriving (Show, Eq)

-- | Head of the clause.
data ClauseHead =
    ClauseDeclHead ClauseVarDecl
  | ClauseVarHead Actor
  deriving (Show, Eq)

-- | Actor variable.
data Actor =
    -- | Free actor variable (and thus concrete w.r.t. the policy under scrutiny).
    Actor ActorName
    -- | Forall quantified actor variable within the current clause.
  | Var Id
  deriving (Show, Eq)
  
-- | Representation of actor names. Unsafe records.
data ActorName =
    -- | Free actor variable.
    ActorName Name
    -- | Free actor type parameter.
  | ActorTypeVar { actorTypeVarAnn  :: Annotation  -- ^ Annotation.
                 , actorTypeVarType :: RefType     -- ^ Type of actor type variable.
                 , actorTypeVarId   :: Id          -- ^ Actor type variable identifier.
                 }
  deriving (Show, Eq)

data Atom = Atom
  { atomAnn    :: Annotation  -- ^ Annotation.
  , atomName   :: Name        -- ^ Atom name.
  , atomActors :: [Actor]     -- ^ Atom actors.
  } deriving (Show, Eq)
 
-- | Policy representation.
type Policy = Exp

instance Annotated Exp where
  ann (Lit       x) = ann x
  ann (NameExp   x) = ann x
  ann a@(Assign {}) = assignAnn a
  ann (PolicyExp x) = ann x

instance Annotated Literal where
  ann = litAnn

instance Annotated Lhs where
  ann = ann . lhsName

instance Annotated AssignOp where
  ann (EqualA x) = x

instance Annotated PolicyExp where
  ann p@(PolicyLit {}) = policyAnn p

instance Annotated Clause where
  ann = clauseAnn

instance Annotated ClauseVarDecl where
  ann = clauseVarDeclAnn

instance Annotated ClauseHead where
  ann (ClauseDeclHead x) = ann x
  ann (ClauseVarHead  x) = ann x

instance Annotated Actor where
  ann (Actor x) = ann x
  ann (Var   x) = ann x

instance Annotated ActorName where
  ann (ActorName x) = ann x
  ann a@(ActorTypeVar {}) = actorTypeVarAnn a

instance Annotated Atom where
  ann = atomAnn
