-- | Paragon Parser. Expressions.
module Language.Java.Paragon.Parser.Expressions where

import Prelude hiding (exp)
import Control.Applicative ((<$>))

import Text.ParserCombinators.Parsec

import Language.Java.Paragon.Annotation
import Language.Java.Paragon.Lexer
import Language.Java.Paragon.Syntax.Expressions hiding (clauseHead)
import Language.Java.Paragon.SrcPos

import Language.Java.Paragon.Parser.Names
import Language.Java.Paragon.Parser.Types
import Language.Java.Paragon.Parser.Separators
import Language.Java.Paragon.Parser.Helpers

stmtExp :: P Exp
stmtExp = assignment

assignment :: P Exp
assignment = do
  startPos <- getStartPos
  lhs <- assignmentLhs
  op <- assignmentOp
  e <- assignmentExp
  endPos <- getEndPos
  return $ Assign (srcSpanToAnn $ mkSrcSpanFromPos startPos endPos) lhs op e

assignmentLhs :: P Lhs
assignmentLhs = NameLhs <$> name expName

assignmentOp :: P AssignOp
assignmentOp =
  EqualA <$> operatorWithSpan Op_Assign

exp :: P Exp
exp = assignmentExp

assignmentExp :: P Exp
assignmentExp =
      Lit <$> literal
  <|> NameExp <$> name expOrLockName
  <|> PolicyExp <$> policyExp
  <?> "expression"

literal :: P Literal
literal =
  tokWithSpanTest $ \t sp ->
    let spa = srcSpanToAnn sp
    in case t of
      IntLit    i -> Just $ Int     spa i
      LongLit   l -> Just $ Long    spa l
      DoubleLit d -> Just $ Double  spa d
      FloatLit  f -> Just $ Float   spa f
      CharLit   c -> Just $ Char    spa c
      StringLit s -> Just $ String  spa s
      BoolLit   b -> Just $ Boolean spa b
      NullLit     -> Just $ Null    spa
      _           -> Nothing

policy :: P Policy
policy = exp

policyExp :: P PolicyExp
policyExp = do
  startPos <- getStartPos
  cls <- braces (seplist1 clause semiColon <|> (colon >> return []))
  endPos <- getEndPos
  return $ PolicyLit (srcSpanToAnn $ mkSrcSpanFromPos startPos endPos) cls

clause :: P Clause
clause = do
  startPos <- getStartPos
  clVarDs <- lopt $ parens $ seplist clauseVarDecl comma
  clHead <- clauseHead
  clAtoms <- colon >> lopt (seplist atom comma)
  endPos <- getEndPos
  -- TODO: genActorVars
  return $ Clause (srcSpanToAnn $ mkSrcSpanFromPos startPos endPos) clVarDs clHead clAtoms

clauseVarDecl :: P ClauseVarDecl
clauseVarDecl = do
  startPos <- getStartPos
  t <- refType
  varId <- ident
  endPos <- getEndPos
  return $ ClauseVarDecl (srcSpanToAnn $ mkSrcSpanFromPos startPos endPos) t varId

clauseHead :: P ClauseHead
clauseHead =
      try (ClauseDeclHead <$> clauseVarDecl)
  <|> ClauseVarHead <$> actor

atom :: P Atom
atom = do
  startPos <- getStartPos
  lName <- name lockName
  actors <- lopt $ parens $ seplist actor comma
  endPos <- getEndPos
  return $ Atom (srcSpanToAnn $ mkSrcSpanFromPos startPos endPos) lName actors

-- Parse everything as actorName and post-process them into Vars.
actor :: P Actor
actor = Actor <$> actorName

actorName :: P ActorName
actorName = ActorName <$> name expName

