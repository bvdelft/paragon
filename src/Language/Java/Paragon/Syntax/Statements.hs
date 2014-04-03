{-# LANGUAGE DeriveDataTypeable #-}
-- | Paragon Abstract Syntax Tree. Statements.
module Language.Java.Paragon.Syntax.Statements
  (
    module Language.Java.Paragon.Syntax.Statements
  , module Language.Java.Paragon.Syntax.Expressions
  ) where

import Data.Data

import Language.Java.Paragon.Syntax.Expressions

import Language.Java.Paragon.Annotated
import Language.Java.Paragon.Annotation

-- | Statements. Unsafe records.
data Stmt =
    -- | Empty statement - semicolon.
    Empty { stmtAnn :: Annotation }
    -- | Expression statement (e.g. assignment, incrementation, decrementation,
    -- method invocation etc.).
  | ExpStmt { stmtAnn :: Annotation  -- ^ Annotation.
            , stmtExp :: Exp         -- ^ Expression.
            }
  deriving (Typeable, Data, Ord, Show, Eq)

instance Annotated Stmt where
  ann = stmtAnn
