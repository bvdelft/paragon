-- | Paragon Parser. Symbols and separators.
module Language.Java.Paragon.Parser.Symbols where

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

dot :: P ()
dot = tok Dot <?> show Dot

colon :: P ()
colon = tok Colon <?> show Colon

ellipsis :: P ()
ellipsis = tok Ellipsis <?> show Ellipsis

