-- | Paragon Parser. Names.
module Language.Java.Paragon.Parser.Names where

import Control.Applicative ((<$>))

import Language.Java.Paragon.Lexer
import Language.Java.Paragon.Syntax.Names
import Language.Java.Paragon.SrcPos

import Language.Java.Paragon.Parser.Separators
import Language.Java.Paragon.Parser.Helpers

ident :: P (Id SrcSpan)
ident =
  tokWithSpanTest $ \t sp ->
    case t of
      IdTok s -> Just $ Id sp s
      _       -> Nothing

name :: ([Id SrcSpan] -> Name SrcSpan) -> P (Name SrcSpan)
name nameFun = nameFun <$> seplist1 ident dot

