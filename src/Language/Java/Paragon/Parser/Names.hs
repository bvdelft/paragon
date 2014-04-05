-- | Paragon Parser. Names.
module Language.Java.Paragon.Parser.Names where

import Control.Applicative ((<$>))

import Language.Java.Paragon.Annotation
import Language.Java.Paragon.Lexer
import Language.Java.Paragon.Syntax.Names

import Language.Java.Paragon.Parser.Symbols
import Language.Java.Paragon.Parser.Helpers

ident :: P Id
ident =
  tokWithSpanTest $ \t sp ->
    case t of
      IdTok s -> Just $ Id (emptyAnnotation { annSrcSpan = sp }) s
      _       -> Nothing

name :: ([Id] -> Name) -> P Name
name nameFun = nameFun <$> seplist1 ident dot

