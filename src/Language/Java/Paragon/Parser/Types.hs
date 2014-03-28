-- | Paragon Parser. Types.
module Language.Java.Paragon.Parser.Types where

import Control.Applicative ((<$>))

import Text.ParserCombinators.Parsec

import Language.Java.Paragon.Annotation
import Language.Java.Paragon.Lexer
import Language.Java.Paragon.Syntax.Types
import Language.Java.Paragon.SrcPos

import Language.Java.Paragon.Parser.Names
import Language.Java.Paragon.Parser.Helpers

ttype :: P Type
ttype =
      PrimType <$> primType
  <|> RefType <$> refType
  <?> "type"

primType :: P PrimType
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

refType :: P RefType
refType = ClassRefType <$> classType

classType :: P ClassType
classType = do
  startPos <- getStartPos
  n <- name qualifiedTypeName
  endPos <- getEndPos
  let cAnn = srcSpanToAnn $ mkSrcSpanFromPos startPos endPos
  return $ ClassType cAnn n []

returnType :: P ReturnType 
returnType =
      VoidType <$> keywordWithSpan KW_Void
  <|> Type <$> ttype
  <?> "type"

