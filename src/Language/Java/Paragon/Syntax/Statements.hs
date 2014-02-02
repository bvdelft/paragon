{-# LANGUAGE DeriveFunctor #-}

-- | Paragon Abstract Syntax Tree. Statements.
module Language.Java.Paragon.Syntax.Statements where

import Language.Java.Paragon.Syntax.Expressions

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

