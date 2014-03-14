-- | Paragon Parser. Types.
module Language.Java.Paragon.Parser.Types where

import Control.Applicative ((<$>))

import Text.ParserCombinators.Parsec

import Language.Java.Paragon.Lexer
import Language.Java.Paragon.Syntax.Types
import Language.Java.Paragon.SrcPos

import Language.Java.Paragon.Parser.Names
import Language.Java.Paragon.Parser.Separators
import Language.Java.Paragon.Parser.Helpers

ttype :: P (Type SrcSpan)
ttype =
      PrimType <$> primType
  <|> RefType <$> refType
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
refType = ClassRefType <$> classType

classType :: P (ClassType SrcSpan)
classType = do
    startPos <- getStartPos
    idsAndTypeArgs <- seplist1 idAndTypeArgs dot
    endPos <- getEndPos
    return $ ClassType (mkSrcSpanFromPos startPos endPos) idsAndTypeArgs
  where idAndTypeArgs :: P (Id SrcSpan, [TypeArgument a])
        idAndTypeArgs = do i <- ident
                           return (i, [])

returnType :: P (ReturnType SrcSpan)
returnType =
      VoidType <$> keywordWithSpan KW_Void
  <|> Type <$> ttype
  <?> "type"

