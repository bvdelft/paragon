module Language.Java.Paragon.NameResolution.Resolvers.Expressions
  (
    rnExp
  ) where

import Language.Java.Paragon.Monad.NameRes
import Language.Java.Paragon.Syntax.Expressions

import Language.Java.Paragon.NameResolution.Expansion
import Language.Java.Paragon.NameResolution.Resolvers.Names

rnExp :: Resolve Exp
rnExp (Lit lit) = return $ Lit lit
rnExp (NameExp name) = do n <- rnName name
                          return $ NameExp n
rnExp assign@(Assign {}) = do lhs <- rnLhs (assignLhs assign)
                              rhs <- rnExp (assignExp assign)
                              return $ assign { assignLhs = lhs
                                              , assignExp = rhs
                                              }
rnExp (PolicyExp policyExp) = do p <- rnPolicyExp policyExp
                                 return $ PolicyExp p

rnLhs :: Resolve Lhs
rnLhs lhs = do name <- rnName (lhsName lhs)
               return $ lhs { lhsName = name }

rnPolicyExp :: Resolve PolicyExp
rnPolicyExp policyLit@(PolicyLit {}) = do
  clauses <- mapM rnClause (policyClauses policyLit)
  return $ policyLit { policyClauses = clauses }

rnClause :: Resolve Clause
rnClause clause = do
  let clauseVarIds  = [clauseVarDeclId v | v <- clauseVarDecls clause]
  let consHeadVarId = case (clauseHead clause) of
                        ClauseDeclHead v -> (clauseVarDeclId v:)
                        _                -> id
  let expns = expansionUnion $ 
                map (mkExpExpansion) $ map idIdent (consHeadVarId clauseVarIds)
  extendExpansion expns $ do
    varDecls <- mapM rnClauseVarDecl (clauseVarDecls clause)
    hdElem   <- rnClauseHead (clauseHead clause)
    atoms    <- mapM rnAtom (clauseAtoms clause)
    return $ clause { clauseVarDecls = varDecls
                    , clauseHead     = hdElem
                    , clauseAtoms    = atoms
                    }

rnClauseVarDecl :: Resolve ClauseVarDecl
rnClauseVarDecl = error "rnClauseVarDecl not implemented"

rnClauseHead :: Resolve ClauseHead
rnClauseHead = error "rnClauseHead not implemented"

rnAtom :: Resolve Atom
rnAtom = error "rnAtom not implemented"
