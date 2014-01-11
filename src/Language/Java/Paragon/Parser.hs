-- | Paragon parsing module. Parser is written using Parsec.
module Language.Java.Paragon.Parser
  (
    parse
  ) where

import Control.Applicative ((<$>))
import Data.Maybe (catMaybes)

import Text.ParserCombinators.Parsec hiding (parse, runParser)
import qualified Text.ParserCombinators.Parsec as Parsec (runParser)

import Language.Java.Paragon.Lexer
import Language.Java.Paragon.Syntax
import Language.Java.Paragon.SrcPos
import Language.Java.Paragon.Parser.Helpers

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

name :: ([Id SrcSpan] -> Name SrcSpan) -> P (Name SrcSpan)
name nameFun = nameFun <$> seplist1 ident period

-- | Parser for the top-level syntax node.
compilationUnit :: P (CompilationUnit SrcSpan)
compilationUnit = do
  startPos <- getParaPos
  pkgDecl <- opt packageDecl <?> "package declaration"
  impDecls <- list importDecl <?> "import declarations"
  typeDecls <- fmap catMaybes (list typeDecl) <?> "type declarations"
  endPos <- getParaPos
  return $ CompilationUnit (mkSrcSpanFromPos startPos endPos)
                           pkgDecl impDecls typeDecls

packageDecl :: P (PackageDecl SrcSpan)
packageDecl = do
  startPos <- getParaPos
  keyword KW_Package
  pName <- name pkgName <?> "package name"
  semiColon
  endPos <- getParaPos
  return $ PackageDecl (mkSrcSpanFromPos startPos endPos) pName

importDecl :: P (ImportDecl SrcSpan)
importDecl = do
    startPos <- getParaPos
    keyword KW_Import
    isStatic <- bopt $ keyword KW_Static
    pkgTypeName <- name ambigName <?> "package/type name"
    hasStar <- bopt $ period >> (tok Op_Star <?> "* or identifier")
    semiColon
    endPos <- getParaPos
    return $ mkImportDecl isStatic hasStar (mkSrcSpanFromPos startPos endPos) pkgTypeName
    -- TODO: fix name types
  where mkImportDecl False False = SingleTypeImport
        mkImportDecl False True  = TypeImportOnDemand
        mkImportDecl True  False = SingleStaticImport
        mkImportDecl True  True  = StaticImportOnDemand

typeDecl :: P (Maybe (TypeDecl SrcSpan))
typeDecl = Just <$> classOrInterfaceDecl
       <|> const Nothing <$> semiColon

classOrInterfaceDecl :: P (TypeDecl SrcSpan)
classOrInterfaceDecl = withModifiers $
  (do startPos <- getParaPos
      cdModsFun <- classDeclModsFun
      endPos <- getParaPos
      return $ \mods ->
        let startPos' = getModifiersStartPos mods startPos
        in ClassTypeDecl (mkSrcSpanFromPos startPos' endPos) (cdModsFun mods))
    <|>
  (do startPos <- getParaPos
      intdModsFun <- interfaceDeclModsFun
      endPos <- getParaPos
      return $ \mods ->
        let startPos' = getModifiersStartPos mods startPos
        in InterfaceTypeDecl (mkSrcSpanFromPos startPos' endPos) (intdModsFun mods))

classDeclModsFun :: P (ModifiersFun (ClassDecl SrcSpan))
classDeclModsFun = normalClassDeclModsFun

interfaceDeclModsFun :: P (ModifiersFun (InterfaceDecl SrcSpan))
interfaceDeclModsFun = do
  startPos <- getParaPos
  keyword KW_Interface
  iName <- ident <?> "interface name"
  openCurly
  closeCurly
  endPos <- getParaPos
  return $ \mods ->
    let startPos' = getModifiersStartPos mods startPos
    in InterfaceDecl (mkSrcSpanFromPos startPos' endPos) mods iName [] [] IB

normalClassDeclModsFun :: P (ModifiersFun (ClassDecl SrcSpan))
normalClassDeclModsFun = do
  startPos <- getParaPos
  keyword KW_Class
  className <- ident <?> "class name"
  body <- classBody <?> "class body"
  endPos <- getParaPos
  return $ \mods ->
    let startPos' = getModifiersStartPos mods startPos
    in ClassDecl (mkSrcSpanFromPos startPos' endPos) mods className [] Nothing [] body

classBody :: P (ClassBody SrcSpan)
classBody = do
  startPos <- getParaPos
  openCurly
  decls <- classBodyDecls
  closeCurly
  endPos <- getParaPos
  return $ ClassBody (mkSrcSpanFromPos startPos endPos) decls

classBodyDecls :: P [Decl SrcSpan]
classBodyDecls = return []

-- Modifiers

-- | There are several syntax tree nodes that have a list of modifiers data field. 
-- This type synonym is for the nodes that have this gap to be filled in (by 'withModifiers').
type ModifiersFun a = [Modifier SrcSpan] -> a

-- | Takes a parser for an entity that expects modifiers before it.
-- Parses zero or more modifiers, runs a given parser which results in
-- a function of type 'ModifiersFun' and applies this function to modifiers.
withModifiers :: P (ModifiersFun a) -> P a
withModifiers pmf = do
  mods <- list modifier <?> "modifiers"
  mf <- pmf
  return $ mf mods

modifier :: P (Modifier SrcSpan)
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

-- Separators

semiColon :: P ()
semiColon = tok SemiColon <?> show SemiColon

period :: P ()
period = tok Period <?> show Period

openCurly :: P ()
openCurly = tok OpenCurly <?> show OpenCurly

closeCurly :: P ()
closeCurly = tok CloseCurly <?> show CloseCurly

