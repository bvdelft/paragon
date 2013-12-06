{-# OPTIONS_GHC
    -fno-warn-orphans
 #-}

-- | Paragon parsing module. Parser is written using Parsec.
module Language.Java.Paragon.Parser
  (
    parse
  , runParser
  ) where

import Control.Applicative (Applicative(..), (<$>), (<*>))
import Control.Monad (ap)
import Data.Maybe (catMaybes, isJust)

import Text.ParserCombinators.Parsec hiding (parse, runParser)
import qualified Text.ParserCombinators.Parsec as Parsec (runParser)
import Text.ParserCombinators.Parsec.Pos (newPos)

import Language.Java.Paragon.Lexer
import Language.Java.Paragon.Syntax
import Language.Java.Paragon.SrcPos

-- | Type synonym for the generic parser on TokenWithSpan and ().
type P = GenParser TokenWithSpan ()

-- | Top-level parsing function.
parse :: String -> String -> Either ParseError (AST SrcSpan)
parse = runParser compilationUnit

-- | Runner for different parsers. Takes a parser, a string to parse and a file name.
runParser :: P a -> String -> String -> Either ParseError a
runParser p input fileName =
  Parsec.runParser (p >>= \r -> eof >> return r) () fileName $ lexer input

-- Parser building blocks. They almost follow the syntax tree structure.

ident :: P (Id SrcSpan)
ident =
  tokWithSpan $ \t sp ->
    case t of
      IdTok s -> Just $ Id sp s
      _       -> Nothing

qIdent :: ([Id SrcSpan] -> QId SrcSpan) -> P (QId SrcSpan)
qIdent nameFun = nameFun <$> seplist1 ident period

-- | Parser for the top-level syntax node.
compilationUnit :: P (CompilationUnit SrcSpan)
compilationUnit = do
  startPos <- getParaPos
  pkgDecl <- opt packageDecl
  impDecls <- list importDecl
  typeDecls <- fmap catMaybes (list typeDecl)
  endPos <- getParaPos
  return $ CompilationUnit (mkSrcSpanFromPos startPos endPos)
                           pkgDecl impDecls typeDecls

packageDecl :: P (PackageDecl SrcSpan)
packageDecl = do
  startPos <- getParaPos
  tok KW_Package
  pName <- qIdent pkgName
  semiColon
  endPos <- getParaPos
  return $ PackageDecl (mkSrcSpanFromPos startPos endPos) pName

importDecl :: P (ImportDecl SrcSpan)
importDecl = do
    startPos <- getParaPos
    tok KW_Import
    isStatic <- bopt $ tok KW_Static
    qId <- qIdent ambigName
    hasStar <- bopt $ period >> tok Op_Star
    semiColon
    endPos <- getParaPos
    return $ mkImportDecl isStatic hasStar (mkSrcSpanFromPos startPos endPos) qId
  where mkImportDecl False False = SingleTypeImport
        mkImportDecl False True  = TypeImportOnDemand
        mkImportDecl True  False = SingleStaticImport
        mkImportDecl True  True  = StaticImportOnDemand

typeDecl :: P (Maybe (TypeDecl SrcSpan))
typeDecl = const Nothing <$> semiColon

-- Separators

semiColon :: P ()
semiColon = tok SemiColon

period :: P ()
period = tok Period

-- Parser helpers

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

-- Name type helpers.
-- Create qualified identifiers of different name types from list of identifiers.

pkgName :: [Id SrcSpan] -> QId SrcSpan
pkgName = mkQId combineSrcSpan PkgName

ambigName :: [Id SrcSpan] -> QId SrcSpan
ambigName = mkQId combineSrcSpan AmbigName

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

