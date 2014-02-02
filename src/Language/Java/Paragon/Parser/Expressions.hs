-- | Paragon Parser. Expressions.
module Language.Java.Paragon.Parser.Expressions where

import Prelude hiding (exp)
import Control.Applicative ((<$>))

import Text.ParserCombinators.Parsec

import Language.Java.Paragon.Lexer
import Language.Java.Paragon.Syntax.Expressions hiding (policyExp, clauseHead, actorName)
import Language.Java.Paragon.Syntax.Names
import Language.Java.Paragon.SrcPos

import Language.Java.Paragon.Parser.Names
import Language.Java.Paragon.Parser.Types
import Language.Java.Paragon.Parser.Separators
import Language.Java.Paragon.Parser.Helpers

stmtExp :: P (Exp SrcSpan)
stmtExp = assignment

assignment :: P (Exp SrcSpan)
assignment = do
  startPos <- getStartPos
  lhs <- assignmentLhs
  op <- assignmentOp
  e <- assignmentExp
  endPos <- getEndPos
  return $ Assign (mkSrcSpanFromPos startPos endPos) lhs op e

assignmentLhs :: P (Lhs SrcSpan)
assignmentLhs = do
  startPos <- getStartPos
  n <- name expName
  endPos <- getEndPos
  return $ NameLhs (mkSrcSpanFromPos startPos endPos) n

assignmentOp :: P (AssignOp SrcSpan)
assignmentOp =
  EqualA <$> operatorWithSpan Op_Assign

exp :: P (Exp SrcSpan)
exp = assignmentExp

assignmentExp :: P (Exp SrcSpan)
assignmentExp =
  (do startPos <- getStartPos
      lit <- literal
      endPos <- getEndPos
      return $ Lit (mkSrcSpanFromPos startPos endPos) lit)
    <|>
  (do startPos <- getStartPos
      n <- name expOrLockName
      endPos <- getEndPos
      return $ NameExp (mkSrcSpanFromPos startPos endPos) n)
    <|>
  (do startPos <- getStartPos
      pExp <- policyExp
      endPos <- getEndPos
      return $ PolicyExp (mkSrcSpanFromPos startPos endPos) pExp)
  <?> "expression"

literal :: P (Literal SrcSpan)
literal =
  tokWithSpanTest $ \t sp ->
    case t of
      IntLit    i -> Just $ Int     sp i
      LongLit   l -> Just $ Long    sp l
      DoubleLit d -> Just $ Double  sp d
      FloatLit  f -> Just $ Float   sp f
      CharLit   c -> Just $ Char    sp c
      StringLit s -> Just $ String  sp s
      BoolLit   b -> Just $ Boolean sp b
      NullLit     -> Just $ Null    sp
      _           -> Nothing

policy :: P (Policy SrcSpan)
policy = exp

policyExp :: P (PolicyExp SrcSpan)
policyExp = do
  startPos <- getStartPos
  cls <- (try $ braces $ seplist clause semiColon) -- TODO: This parses {}. Should it?
     <|> (braces colon >> return [])
  endPos <- getEndPos
  return $ PolicyLit (mkSrcSpanFromPos startPos endPos) cls

clause :: P (Clause SrcSpan)
clause = do
  startPos <- getStartPos
  clVarDs <- lopt $ parens $ seplist clauseVarDecl comma
  clHead <- clauseHead
  clAtoms <- colon >> lopt (seplist atom comma) -- TODO: Is colon required?
  endPos <- getEndPos
  -- TODO: genActorVars
  return $ Clause (mkSrcSpanFromPos startPos endPos) clVarDs clHead clAtoms

clauseVarDecl :: P (ClauseVarDecl SrcSpan)
clauseVarDecl = do
  startPos <- getStartPos
  t <- refType
  varId <- ident
  endPos <- getEndPos
  return $ ClauseVarDecl (mkSrcSpanFromPos startPos endPos) t varId

clauseHead :: P (ClauseHead SrcSpan)
clauseHead =
  (try $ do
    startPos <- getStartPos
    clVarDecl <- clauseVarDecl
    endPos <- getEndPos
    return $ ClauseDeclHead (mkSrcSpanFromPos startPos endPos) clVarDecl)
    <|>
  (do startPos <- getStartPos
      a <- actor
      endPos <- getEndPos
      return $ ClauseVarHead (mkSrcSpanFromPos startPos endPos) a)

atom :: P (Atom SrcSpan)
atom = do
  startPos <- getStartPos
  lName <- name lockName
  actors <- lopt $ parens $ seplist actor comma
  endPos <- getEndPos
  return $ Atom (mkSrcSpanFromPos startPos endPos) lName actors

-- Parse everything as actorName and post-process them into Vars.
actor :: P (Actor SrcSpan)
actor = do
  startPos <- getStartPos
  aName <- actorName
  endPos <- getEndPos
  return $ Actor (mkSrcSpanFromPos startPos endPos) aName

actorName :: P (ActorName SrcSpan)
actorName = do
  startPos <- getStartPos
  eName <- name expName
  endPos <- getEndPos
  return $ ActorName (mkSrcSpanFromPos startPos endPos) eName

