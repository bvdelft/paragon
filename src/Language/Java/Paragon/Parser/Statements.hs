-- | Paragon Parser. Statements.
module Language.Java.Paragon.Parser.Statements where

import Text.ParserCombinators.Parsec

import Language.Java.Paragon.Syntax.Statements hiding (stmtExp)
import Language.Java.Paragon.SrcPos

import Language.Java.Paragon.Parser.Expressions
import Language.Java.Paragon.Parser.Separators
import Language.Java.Paragon.Parser.Helpers

stmt :: P (Stmt SrcSpan)
stmt =
  (do startPos <- getStartPos
      semiColon
      endPos <- getEndPos
      return $ Empty (mkSrcSpanFromPos startPos endPos))
    <|>
  (do startPos <- getStartPos
      e <- stmtExp
      semiColon
      endPos <- getEndPos
      return $ ExpStmt (mkSrcSpanFromPos startPos endPos) e)
