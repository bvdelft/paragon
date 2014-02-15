-- | Paragon parsing module. Parser is written using Parsec.
module Language.Java.Paragon.Parser
  (
    parse
  , runParser
  ) where

import Prelude hiding (exp)
import Control.Applicative ((<$>))
import Data.Maybe (catMaybes)

import Text.ParserCombinators.Parsec hiding (parse, runParser)
import qualified Text.ParserCombinators.Parsec as Parsec (runParser)

import Language.Java.Paragon.Lexer
import Language.Java.Paragon.Syntax
import Language.Java.Paragon.SrcPos

import Language.Java.Paragon.Parser.Names
import Language.Java.Paragon.Parser.Types
import Language.Java.Paragon.Parser.Statements
import Language.Java.Paragon.Parser.Expressions
import Language.Java.Paragon.Parser.Separators
import Language.Java.Paragon.Parser.Helpers

-- | Top-level parsing function. Takes a string to parse and a file name.
parse :: String -> String -> Either ParseError (AST SrcSpan)
parse = runParser compilationUnit

-- | Runner for different parsers. Takes a parser, a string to parse and a file name.
runParser :: P a -> String -> String -> Either ParseError a
runParser p input fileName = Parsec.runParser (p >>= \r -> eof >> return r) initState fileName toks
  where initState = SrcPos fileName 1 1
        toks = lexer input

-- Parser building blocks. They almost follow the syntax tree structure.

-- | Parser for the top-level syntax node.
compilationUnit :: P (CompilationUnit SrcSpan)
compilationUnit = do
  startPos <- getStartPos
  pkgDecl <- opt packageDecl
  impDecls <- list importDecl
  typeDecls <- fmap catMaybes (list typeDecl)
  endPos <- getEndPos
  return $ CompilationUnit (mkSrcSpanFromPos startPos endPos)
                           pkgDecl impDecls typeDecls

packageDecl :: P (PackageDecl SrcSpan)
packageDecl = do
  startPos <- getStartPos
  keyword KW_Package
  pName <- name pkgName <?> "package name"
  semiColon
  endPos <- getEndPos
  return $ PackageDecl (mkSrcSpanFromPos startPos endPos) pName
  <?> "package declaration"

importDecl :: P (ImportDecl SrcSpan)
importDecl = do
    startPos <- getStartPos
    keyword KW_Import
    isStatic <- bopt $ keyword KW_Static
    pkgTypeName <- name ambigName <?> "package/type name"
    hasStar <- bopt $ period >> (tok Op_Star <?> "* or identifier")
    semiColon
    endPos <- getEndPos
    return $ mkImportDecl isStatic hasStar (mkSrcSpanFromPos startPos endPos) pkgTypeName
    <?> "import declaration"
  where mkImportDecl False False sp n = SingleTypeImport     sp (typeName $ flattenName n)
        mkImportDecl False True  sp n = TypeImportOnDemand   sp (pkgOrTypeName $ flattenName n)
        mkImportDecl True  False sp n = SingleStaticImport   sp (mkSingleStaticImport n)
        mkImportDecl True  True  sp n = StaticImportOnDemand sp (typeName $ flattenName n)

        mkSingleStaticImport n =
          let flName = flattenName n
              (lastI, initN) = (last flName, init flName)
          in Name (nameAnn n) lastI (nameType n) (if null initN
                                                    then Nothing
                                                    else (Just $ typeName initN))

typeDecl :: P (Maybe (TypeDecl SrcSpan))
typeDecl = Just <$> classOrInterfaceDecl
       <|> const Nothing <$> semiColon
  <?> "type declaration"

classOrInterfaceDecl :: P (TypeDecl SrcSpan)
classOrInterfaceDecl = withModifiers $
  (do cdModsFun <- classDeclModsFun
      return $ \mods -> ClassTypeDecl (cdModsFun mods))
    <|>
  (do intdModsFun <- interfaceDeclModsFun
      return $ \mods -> InterfaceTypeDecl (intdModsFun mods))

classDeclModsFun :: P (ModifiersFun (ClassDecl SrcSpan))
classDeclModsFun = normalClassDeclModsFun

normalClassDeclModsFun :: P (ModifiersFun (ClassDecl SrcSpan))
normalClassDeclModsFun = do
  startPos <- getStartPos
  keyword KW_Class
  classId <- ident <?> "class name"
  body <- classBody
  endPos <- getEndPos
  return $ \mods ->
    let startPos' = getModifiersStartPos mods startPos
    in ClassDecl (mkSrcSpanFromPos startPos' endPos) mods classId [] Nothing [] body

interfaceDeclModsFun :: P (ModifiersFun (InterfaceDecl SrcSpan))
interfaceDeclModsFun = do
  startPos <- getStartPos
  keyword KW_Interface
  iId <- ident <?> "interface name"
  openCurly
  closeCurly
  endPos <- getEndPos
  return $ \mods ->
    let startPos' = getModifiersStartPos mods startPos
    in InterfaceDecl (mkSrcSpanFromPos startPos' endPos) mods iId [] [] IB

classBody :: P (ClassBody SrcSpan)
classBody = do
  startPos <- getStartPos
  decls <- braces classBodyDecls
  endPos <- getEndPos
  return $ ClassBody (mkSrcSpanFromPos startPos endPos) decls
  <?> "class body"

classBodyDecls :: P [ClassBodyDecl SrcSpan]
classBodyDecls = list classBodyDecl

classBodyDecl :: P (ClassBodyDecl SrcSpan)
classBodyDecl = withModifiers (do
  membDeclModsFun <- memberDeclModsFun
  return $ \mods -> MemberDecl (membDeclModsFun mods))
  <?> "class body declaration"

memberDeclModsFun :: P (ModifiersFun (MemberDecl SrcSpan))
memberDeclModsFun = try (fieldDeclModsFun varDecl)
                <|> methodDeclModsFun

fieldDeclModsFun :: P (VarDecl SrcSpan) -> P (ModifiersFun (MemberDecl SrcSpan))
fieldDeclModsFun varDeclFun = do
  startPos <- getStartPos
  t <- ttype
  varDs <- varDecls varDeclFun
  semiColon
  endPos <- getEndPos
  return $ \mods ->
    let startPos' = getModifiersStartPos mods startPos
    in FieldDecl (mkSrcSpanFromPos startPos' endPos) mods t varDs

methodDeclModsFun :: P (ModifiersFun (MemberDecl SrcSpan))
methodDeclModsFun = do
  startPos <- getStartPos
  retT <- returnType
  mId <- ident <?> "method name"
  openParen
  closeParen
  body <- methodBody
  endPos <- getEndPos
  return $ \mods ->
    let startPos' = getModifiersStartPos mods startPos
    in MethodDecl (mkSrcSpanFromPos startPos' endPos) mods [] retT mId [] body

-- | Takes 'VarDecl' parser to handle restrictions on field declarations in interfaces
-- (required presence of initializer).
varDecls :: P (VarDecl SrcSpan) -> P [VarDecl SrcSpan]
varDecls varDeclFun = varDeclFun `sepBy1` comma

varDecl :: P (VarDecl SrcSpan)
varDecl = do
  startPos <- getStartPos
  varId <- ident <?> "variable/field name"
  vInit <- opt $ tok Op_Assign >> varInit
  endPos <- getEndPos
  return $ VarDecl (mkSrcSpanFromPos startPos endPos) varId vInit

methodBody :: P (MethodBody SrcSpan)
methodBody = do
  startPos <- getStartPos
  mBlock <- const Nothing <$> semiColon
        <|> Just <$> block
  endPos <- getEndPos
  return $ MethodBody (mkSrcSpanFromPos startPos endPos) mBlock

varInit :: P (VarInit SrcSpan)
varInit = InitExp <$> exp

block :: P (Block SrcSpan)
block = do
  startPos <- getStartPos
  blStmts <- braces $ list blockStmt
  endPos <- getEndPos
  return $ Block (mkSrcSpanFromPos startPos endPos) blStmts

blockStmt :: P (BlockStmt SrcSpan)
blockStmt =
  (withModifiers (try $ do
      startPos <- getStartPos
      (t, varDs) <- localVarDecl
      semiColon
      endPos <- getEndPos
      return $ \mods ->
        let startPos' = getModifiersStartPos mods startPos
        in LocalVars (mkSrcSpanFromPos startPos' endPos) mods t varDs)
    <?> "local variable declaration")
    <|>
  (BlockStmt <$> stmt <?> "statement")

localVarDecl :: P (Type SrcSpan, [VarDecl SrcSpan])
localVarDecl = do
  t <- ttype
  varDs <- varDecls varDecl
  return (t, varDs)

-- Modifiers

-- | There are several syntax tree nodes that have a list of modifiers data field. 
-- This type synonym is for the nodes that have this gap to be filled in (by 'withModifiers').
-- When 'ModifiersFun' and 'withModifiers' are used, be careful to deal correctly with the
-- source span. Look at the example of how to fix it in existing parsing functions.
type ModifiersFun a = [Modifier SrcSpan] -> a

-- | Takes a parser for an entity that expects modifiers before it.
-- Parses zero or more modifiers, runs a given parser which results in
-- a function of type 'ModifiersFun' and applies this function to modifiers.
withModifiers :: P (ModifiersFun a) -> P a
withModifiers pmf = do
  mods <- list modifier
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
  <|> (do startPos <- getStartPos
          tok Question
          p <- policy
          endPos <- getEndPos
          return $ Reads (mkSrcSpanFromPos startPos endPos) p)
  <|> (do startPos <- getStartPos
          tok Op_Bang
          p <- policy
          endPos <- getEndPos
          return $ Writes (mkSrcSpanFromPos startPos endPos) p)
  <?> "modifier"

