{-# OPTIONS_GHC
    -fno-warn-orphans
 #-}

-- | Helper functions for Paragon parser.
module Language.Java.Paragon.Parser.Helpers
  (
    P
  , tok
  , tokWithSpan
  , tokWithSpanDesc
  , tokWithSpanTest
  , keyword
  , keywordWithSpan
  , operator
  , operatorWithSpan
    -- * Source positions
  , getStartPos
  , getEndPos
  , srcPosToParsec
    -- * Parser combinators
  , opt
  , bopt
  , lopt
  , list
  , list1
  , seplist
  , seplist1
  ) where

import Control.Monad (ap, unless)
import Control.Applicative (Applicative(..), (<$>), (<*>))
import Data.Maybe (isJust)

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos (newPos)

import Language.Java.Paragon.Annotation
import Language.Java.Paragon.Lexer
import Language.Java.Paragon.SrcPos

-- | Type synonym for the generic parser on TokenWithSpan and SrcPos (as user state).
-- User state represents the end position of the last parsed token, used in 'getEndPos'.
-- This is done for building correct source spans, because otherwise Parsec jumps to the start position of the next token.
type P = GenParser TokenWithSpan SrcPos

instance Applicative (GenParser s a) where
  pure  = return
  (<*>) = ap

-- | Matches a given token.
tok :: Token -> P ()
tok t = do
    fileName <- getFileName
    updateUserState fileName
    token (show . twsTok) (posFromTok fileName) testT
  where testT tws = if t == twsTok tws then Just () else Nothing

-- | Matches a given token and returns a source span.
tokWithSpan :: Token -> P Annotation
tokWithSpan t = do
  srcSpan <- tokWithSpanTest (\t' sp -> if t == t' then Just sp else Nothing)
  return $ srcSpanToAnn srcSpan

-- | Matches a given token and returns a source span.
-- Uses passed description string when reporting errors.
tokWithSpanDesc :: String -> Token -> P Annotation
tokWithSpanDesc desc t = tokWithSpan t <?> desc ++ " " ++ show t

-- | Parses a token which is accepted by a given function.
tokWithSpanTest :: (Token -> SrcSpan -> Maybe a) -> P a
tokWithSpanTest test = do
    fileName <- getFileName
    updateUserState fileName
    token (show . twsTok) (posFromTok fileName) (testT fileName)
  where testT fileName (TokWSpan t sp) = test t (sp { srcSpanFileName = fileName })
        -- testT also fixes the file name in the source span

-- | Returns the Parsec source position for a given file name and token.
-- One of the main purposes: fix the file name in the token with the given name.
posFromTok :: String -> TokenWithSpan -> SourcePos
posFromTok fileName (TokWSpan _ sp) =
  srcPosToParsec $ srcSpanToStartPos (sp { srcSpanFileName = fileName })

-- | Sets user state to the end position of the token to be parsed (first in the input).
-- Fixes the file name in the token with the given name.
updateUserState :: String -> P ()
updateUserState fileName = do
  ts <- getInput
  unless (null ts) $
    setState (srcSpanToEndPos $ (twsSrcSpan $ head ts) { srcSpanFileName = fileName } )

-- | Matches a given keyword with better error message in case of failure.
keyword :: Token -> P ()
keyword t = tok t <?> "keyword " ++ show t

-- | Matches a given keyword and returns a source span for this keyword
-- as part of the empty annotation.
keywordWithSpan :: Token -> P Annotation
keywordWithSpan = tokWithSpanDesc "keyword"

-- | Matches a given operator with better error message in case of failure.
operator :: Token -> P ()
operator t = tok t <?> "operator " ++ show t

-- | Matches a given operator and returns a source span for this operator.
operatorWithSpan :: Token -> P Annotation
operatorWithSpan = tokWithSpanDesc "operator"

-- Source positions

-- | Returns the current source position.
getStartPos :: P SrcPos
getStartPos = do
  pos <- getPosition
  return (parsecToSrcPos pos)

-- | Returns the source position from the user state which points to the end of last parsed token.
getEndPos :: P SrcPos
getEndPos = getState

-- | Returns the source file name.
getFileName :: P String
getFileName = sourceName <$> getPosition

-- | Converts Parsec source position to Paragon representation.
parsecToSrcPos :: SourcePos -> SrcPos
parsecToSrcPos pos = SrcPos (sourceName pos)
                            (sourceLine pos)
                            (sourceColumn pos)

-- | Converts Paragon source position to Parsec representation.
srcPosToParsec :: SrcPos -> SourcePos
srcPosToParsec (SrcPos fileName l c) = newPos fileName l c

-- Parser combinators

-- | Optional parser.
opt :: P a -> P (Maybe a)
opt = optionMaybe

-- | Optional parser which indicates its success as a boolean.
bopt :: P a -> P Bool
bopt p = isJust <$> opt p

-- | Optional list parser which indicates its failure as an empty list.
lopt :: P [a] -> P [a]
lopt p = do
  mas <- opt p
  case mas of
    Nothing -> return []
    Just as -> return as

-- | Parses zero or more occurences with a given parser.
list :: P a -> P [a]
list = option [] . list1

-- | Parses one or more occurences with a given parser.
list1 :: P a -> P [a]
list1 = many1

-- | Parses zero or more occurences with a parser given as the first argument,
-- separated by a separator given as the second argument.
seplist :: P a -> P sep -> P [a]
seplist p sep = option [] $ seplist1 p sep

-- | Parses one or more occurences with a parser given as the first argument,
-- separated by a separator given as the second argument.
-- Doesn't consume a separator if it occures at the end!
seplist1 :: P a -> P sep -> P [a]
seplist1 p sep = do
  a <- p
  try (do _ <- sep
          as <- seplist1 p sep
          return (a:as))
    <|> return [a]

