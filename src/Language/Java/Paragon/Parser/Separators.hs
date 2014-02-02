-- | Paragon Parser. Separators.
module Language.Java.Paragon.Parser.Separators where

import Text.ParserCombinators.Parsec

import Language.Java.Paragon.Lexer
import Language.Java.Paragon.Parser.Helpers

parens :: P a -> P a
parens = between openParen closeParen

openParen :: P ()
openParen = tok OpenParen <?> show OpenParen

closeParen :: P ()
closeParen = tok CloseParen <?> show CloseParen

braces :: P a -> P a
braces = between openCurly closeCurly

openCurly :: P ()
openCurly = tok OpenCurly <?> show OpenCurly

closeCurly :: P ()
closeCurly = tok CloseCurly <?> show CloseCurly

semiColon :: P ()
semiColon = tok SemiColon <?> show SemiColon

comma :: P ()
comma = tok Comma <?> show Comma

period :: P ()
period = tok Period <?> show Period

colon :: P ()
colon = tok Colon <?> show Colon

