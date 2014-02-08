{-# LANGUAGE DeriveFunctor #-}

-- | Paragon Abstract Syntax Tree. Statements.
module Language.Java.Paragon.Syntax.Statements
  (
    module Language.Java.Paragon.Syntax.Statements
  , module Language.Java.Paragon.Syntax.Expressions
  ) where

import Language.Java.Paragon.Syntax.Expressions

-- | Statements. Unsafe records.
data Stmt a =
    -- | Empty statement - semicolon.
    Empty { stmtAnn :: a }
    -- | Expression statement (e.g. assignment, incrementation, decrementation,
    -- method invocation etc.).
  | ExpStmt { stmtAnn :: a      -- ^ Annotation.
            , stmtExp :: Exp a  -- ^ Expression.
            }
  deriving (Show, Eq, Functor)

