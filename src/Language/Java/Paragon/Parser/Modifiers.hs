-- | Paragon Parser. Modifiers.
module Language.Java.Paragon.Parser.Modifiers
  (
    ModifiersFun
  , withModifiers
  , modifier
  , getModifiersStartPos
  ) where

import Control.Applicative ((<$>))
import Text.ParserCombinators.Parsec

import Language.Java.Paragon.Annotation
import Language.Java.Paragon.Lexer
import Language.Java.Paragon.Syntax.Modifiers
import Language.Java.Paragon.SrcPos
import Language.Java.Paragon.Annotated

import Language.Java.Paragon.Parser.Expressions
import Language.Java.Paragon.Parser.Helpers

-- | There are several syntax tree nodes that have a list of modifiers data field. 
-- This type synonym is for the nodes that have this gap to be filled in (by 'withModifiers').
-- When 'ModifiersFun' and 'withModifiers' are used, be careful to deal correctly with the
-- source span. Look at the example of how to fix it in existing parsing functions.
type ModifiersFun a = [Modifier] -> a

-- | Takes a parser for an entity that expects modifiers before it.
-- Parses zero or more modifiers, runs a given parser which results in
-- a function of type 'ModifiersFun' and applies this function to modifiers.
withModifiers :: P (ModifiersFun a) -> P a
withModifiers pmf = do
  mods <- list modifier
  mf <- pmf
  return $ mf mods

modifier :: P Modifier
modifier =
      Public       <$> keywordWithSpan KW_Public
  <|> Protected    <$> keywordWithSpan KW_Protected
  <|> Private      <$> keywordWithSpan KW_Private
  <|> Static       <$> keywordWithSpan KW_Static
  <|> Abstract     <$> keywordWithSpan KW_Abstract
  <|> Final        <$> keywordWithSpan KW_Final
  <|> Native       <$> keywordWithSpan KW_Native
  <|> Synchronized <$> keywordWithSpan KW_Synchronized
  <|> Transient    <$> keywordWithSpan KW_Transient
  <|> Volatile     <$> keywordWithSpan KW_Volatile
  <|> StrictFP     <$> keywordWithSpan KW_Strictfp
  -- Paragon specific
  <|> Typemethod   <$> keywordWithSpan KW_P_Typemethod
  <|> Reflexive    <$> keywordWithSpan KW_P_Reflexive
  <|> Transitive   <$> keywordWithSpan KW_P_Transitive
  <|> Symmetric    <$> keywordWithSpan KW_P_Symmetric
  <|> Readonly     <$> keywordWithSpan KW_P_Readonly
  <|> Notnull      <$> keywordWithSpan KW_P_Notnull
  <|> (do startPos <- getStartPos
          tok Question
          p <- policy
          endPos <- getEndPos
          return $ Reads (srcSpanToAnn $ mkSrcSpanFromPos startPos endPos) p)
  <|> (do startPos <- getStartPos
          tok Op_Bang
          p <- policy
          endPos <- getEndPos
          return $ Writes (srcSpanToAnn $ mkSrcSpanFromPos startPos endPos) p)
  <?> "modifier"

-- | Takes a list of modifiers annotated with source spans and a default starting position.
-- If there are no modifiers - returns the default, otherwise - returns the starting 
-- position of the first modifier.
getModifiersStartPos :: [Modifier] -> SrcPos -> SrcPos
getModifiersStartPos []    defaultSrcPos = defaultSrcPos
getModifiersStartPos (m:_) _             = srcSpanToStartPos (annSrcSpan $ getAnn m)

