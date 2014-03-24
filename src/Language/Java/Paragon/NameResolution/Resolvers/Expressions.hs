module Language.Java.Paragon.NameResolution.Resolvers.Expressions
  (
    -- * Resolver
    rnExp
  ) where

import Language.Java.Paragon.Monad.NameRes
import Language.Java.Paragon.Syntax.Expressions

import Language.Java.Paragon.NameResolution.Expansion
import Language.Java.Paragon.NameResolution.Resolvers.Names
import Language.Java.Paragon.NameResolution.Resolvers.Types

-- | Resolve an expression.
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

-- | Resolve the left-hand side of an assignment expression.
rnLhs :: Resolve Lhs
rnLhs lhs = do name <- rnName (lhsName lhs)
               return $ lhs { lhsName = name }

-- | Resolve a policy by mapping the clause resolver over its clauses.
rnPolicyExp :: Resolve PolicyExp
rnPolicyExp policyLit@(PolicyLit {}) = do
  clauses <- mapM rnClause (policyClauses policyLit)
  return $ policyLit { policyClauses = clauses }

-- | Resolve a clause. Both the variable declarations and the clause head might
-- introduce new names for which the expansion needs to be extended accordingly.
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
rnClauseVarDecl clauseVarDecl = do
  refType <- rnRefType (clauseVarDeclType clauseVarDecl)
  return $ clauseVarDecl { clauseVarDeclType = refType }

rnClauseHead :: Resolve ClauseHead
rnClauseHead (ClauseDeclHead declHead) =
  fmap ClauseDeclHead $ rnClauseVarDecl declHead
rnClauseHead (ClauseVarHead varHead) = fmap ClauseVarHead $ rnActor varHead

rnActor :: Resolve Actor
rnActor (Actor actorName) = fmap Actor $ rnActorName actorName
rnActor (Var i) = return $ Var i

rnActorName :: Resolve ActorName
rnActorName (ActorName name) = fmap ActorName $ rnName name
rnActorName typeVar@(ActorTypeVar {}) = do
  varType <- rnRefType (actorTypeVarType typeVar)
  return $ typeVar { actorTypeVarType = varType }

rnAtom :: Resolve Atom
rnAtom = error "rnAtom not implemented"
