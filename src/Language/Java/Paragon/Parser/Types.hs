-- | Paragon Parser. Types.
module Language.Java.Paragon.Parser.Types where

import Control.Applicative ((<$>))

import Text.ParserCombinators.Parsec

import Language.Java.Paragon.Lexer
import Language.Java.Paragon.Syntax.Types
import Language.Java.Paragon.SrcPos

import Language.Java.Paragon.Parser.Names
import Language.Java.Paragon.Parser.Helpers

ttype :: P (Type SrcSpan)
ttype =
  (do startPos <- getStartPos
      t <- primType
      endPos <- getEndPos
      return $ PrimType (mkSrcSpanFromPos startPos endPos) t)
    <|>
  (do startPos <- getStartPos
      t <- refType
      endPos <- getEndPos
      return $ RefType (mkSrcSpanFromPos startPos endPos) t)
  <?> "type"

primType :: P (PrimType SrcSpan)
primType =
      BooleanT <$> keywordWithSpan KW_Boolean
  <|> ByteT    <$> keywordWithSpan KW_Byte
  <|> ShortT   <$> keywordWithSpan KW_Short
  <|> IntT     <$> keywordWithSpan KW_Int
  <|> LongT    <$> keywordWithSpan KW_Long
  <|> CharT    <$> keywordWithSpan KW_Char
  <|> FloatT   <$> keywordWithSpan KW_Float
  <|> DoubleT  <$> keywordWithSpan KW_Double
  -- Paragon specific
  <|> PolicyT  <$> keywordWithSpan KW_P_Policy

refType :: P (RefType SrcSpan)
refType = do
  startPos <- getStartPos
  ct <- classType
  endPos <- getEndPos
  return $ ClassRefType (mkSrcSpanFromPos startPos endPos) ct

classType :: P (ClassType SrcSpan)
classType = do
  startPos <- getStartPos
  n <- name typeName
  endPos <- getEndPos
  return $ ClassType (mkSrcSpanFromPos startPos endPos) n []

returnType :: P (ReturnType SrcSpan)
returnType =
      VoidType <$> keywordWithSpan KW_Void
  <|> (do startPos <- getStartPos
          t <- ttype
          endPos <- getEndPos
          return $ Type (mkSrcSpanFromPos startPos endPos) t)
  <?> "type"

