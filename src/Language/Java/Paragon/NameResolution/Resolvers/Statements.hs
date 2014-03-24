module Language.Java.Paragon.NameResolution.Resolvers.Statements
  (
    -- * Resolver
    rnStmt
  ) where

import Language.Java.Paragon.Monad.NameRes
import Language.Java.Paragon.Syntax.Statements

import Language.Java.Paragon.NameResolution.Resolvers.Expressions

-- | Resolve statements.           
rnStmt :: Resolve Stmt
rnStmt empty@(Empty {}) = return empty
rnStmt expStmt@(ExpStmt {}) = do e <- rnExp (stmtExp expStmt)
                                 return $ expStmt { stmtExp = e }
