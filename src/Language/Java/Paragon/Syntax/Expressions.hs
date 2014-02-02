{-# LANGUAGE DeriveFunctor #-}

-- | Paragon Abstract Syntax Tree. Expressions.
module Language.Java.Paragon.Syntax.Expressions
  (
    module Language.Java.Paragon.Syntax.Expressions
  , module Language.Java.Paragon.Syntax.Names
  , module Language.Java.Paragon.Syntax.Types
  ) where

import Language.Java.Paragon.Syntax.Names
import Language.Java.Paragon.Syntax.Types

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
    -- | Policy expression.
  | PolicyExp { expAnn    :: a
              , policyExp :: PolicyExp a
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
    ClauseDeclHead { clauseHeadAnn     :: a                -- ^ Annotation.
                   , clauseHeadVarDecl :: ClauseVarDecl a
                   }
  | ClauseVarHead { clauseHeadAnn   :: a
                  , clauseHeadActor :: Actor a
                  }
  deriving (Show, Eq, Functor)

-- | Actor variable.
data Actor a =
    -- | Free actor variable (and thus concrete w.r.t. the policy under scrutiny).
    Actor { actorAnn  :: a            -- ^ Annotation.
          , actorName :: ActorName a  -- ^ Name of free actor variable.
          }
    -- | Forall qualified actor variable within the current clause.
  | Var { actorAnn   :: a
        , actorVarId :: Id a  -- ^ Quantified variable identifier.
        }
  deriving (Show, Eq, Functor)

-- | Representation of actor names.
data ActorName a =
    -- | Free actor variable.
    ActorName { actorNameAnn  :: a       -- ^ Annotation.
              , actorNameName :: Name a  -- ^ Name of free actor variable.
              }
    -- | Free actor type parameter.
  | ActorTypeVar { actorNameAnn     :: a
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

