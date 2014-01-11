{-# OPTIONS_GHC
    -fno-warn-orphans
 #-}

-- | Helper functions for Paragon parser.
module Language.Java.Paragon.Parser.Helpers where

import Control.Monad (ap)
import Control.Applicative (Applicative(..), (<$>), (<*>))
import Data.Maybe (isJust)

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos (newPos)

import Language.Java.Paragon.Lexer
import Language.Java.Paragon.Syntax
import Language.Java.Paragon.SrcPos

-- | Type synonym for the generic parser on TokenWithSpan and ().
type P = GenParser TokenWithSpan ()

instance Applicative (GenParser s a) where
  pure  = return
  (<*>) = ap

-- | Matches a given token.
tok :: Token -> P ()
tok t = do
    fileName <- getFileName
    token (show . twsTok) (posFromTok fileName) testT
  where testT tws = if t == twsTok tws then Just () else Nothing

-- | Parses a token which is accepted by a given function.
tokWithSpan :: (Token -> SrcSpan -> Maybe a) -> P a
tokWithSpan test = do
    fileName <- getFileName
    token (show . twsTok) (posFromTok fileName) (testT fileName)
  where testT fileName (TokWSpan t sp) = test t (sp { srcSpanFileName = fileName })
        -- testT also fixes the file name in the source span

-- | Returns the Parsec source position for a given file name and token.
-- One of the main purposes: fix the file name in the token with the given name.
posFromTok :: String -> TokenWithSpan -> SourcePos
posFromTok fileName (TokWSpan _ sp) =
  srcPosToParsec $ srcSpanToPos (sp { srcSpanFileName = fileName })

-- | Matches a given keyword with better error message in case of failure.
keyword :: Token -> P ()
keyword t = tok t <?> "keyword " ++ show t

-- | Matches a given keyword and returns a source span for this keyword.
keywordWithSpan :: Token -> P SrcSpan
keywordWithSpan k =
  (tokWithSpan $ \t sp -> if t == k then Just sp else Nothing) <?> "keyword " ++ show k

-- Source positions

-- | Returns the current source position.
getParaPos :: P SrcPos
getParaPos = do
  pos <- getPosition
  return (parsecToSrcPos pos)

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

-- | Takes a list of modifiers annotated with source spans and a default starting position.
-- If there are no modifiers - returns the default, otherwise - returns the starting 
-- position of the first modifier.
getModifiersStartPos :: [Modifier SrcSpan] -> SrcPos -> SrcPos
getModifiersStartPos []    defaultSrcPos = defaultSrcPos
getModifiersStartPos (m:_) _             = srcSpanToPos (ann m)

-- Parser combinators

-- | Optional parser.
opt :: P a -> P (Maybe a)
opt = optionMaybe

-- | Optional parser which indicates its success as a boolean.
bopt :: P a -> P Bool
bopt p = isJust <$> opt p

-- | Parses zero or more occurences with a given parser.
list :: P a -> P [a]
list = option [] . list1

-- | Parses one or more occurences with a given parser.
list1 :: P a -> P [a]
list1 = many1

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

