{-# LANGUAGE Rank2Types #-}
-- | Paragon parsing module. Parser is written using Parsec.
module Language.Java.Paragon.Parser
  (
    parse
  , runParser
  , parseError
  ) where

import Prelude hiding (exp)
import Control.Applicative ((<$>))
import Control.Monad (when, void)
import Data.Maybe (catMaybes, fromJust)
import Data.List (find)

import Text.ParserCombinators.Parsec hiding (parse, runParser)
import qualified Text.ParserCombinators.Parsec as Parsec (runParser)

import Language.Java.Paragon.Annotation
import Language.Java.Paragon.Error
import Language.Java.Paragon.Lexer
import Language.Java.Paragon.Syntax
import Language.Java.Paragon.SrcPos

import Language.Java.Paragon.Parser.Names
import Language.Java.Paragon.Parser.Types
import Language.Java.Paragon.Parser.Statements
import Language.Java.Paragon.Parser.Expressions
import Language.Java.Paragon.Parser.Modifiers
import Language.Java.Paragon.Parser.Symbols
import Language.Java.Paragon.Parser.Helpers

-- | Top-level parsing function. Takes a string to parse and a file name.
parse :: String -> String -> Either ParseError AST
parse = runParser compilationUnit

-- | Runner for different parsers. Takes a parser, a string to parse and a file name.
runParser :: P a -> String -> String -> Either ParseError a
runParser p input fileName = Parsec.runParser (p >>= \r -> eof >> return r) initState fileName toks
  where initState = SrcPos fileName 1 1
        toks = lexer input

-- | Convert parse error to Paragon error.
parseError :: ParseError -> MkError
parseError pe = mkError $ defaultError { pretty = show pe }

-- Parser building blocks. They almost follow the syntax tree structure.

-- | Parser for the top-level syntax node.
compilationUnit :: P CompilationUnit
compilationUnit = do
  startPos <- getStartPos
  pkgDecl <- opt packageDecl
  impDecls <- list importDecl
  typeDecls <- fmap catMaybes (list typeDecl)
  endPos <- getEndPos
  return $ CompilationUnit (srcSpanToAnn $ mkSrcSpanFromPos startPos endPos)
                           pkgDecl impDecls typeDecls

packageDecl :: P PackageDecl
packageDecl = do
  startPos <- getStartPos
  keyword KW_Package
  pName <- name pkgName <?> "package name"
  semiColon
  endPos <- getEndPos
  return $ PackageDecl (srcSpanToAnn $ mkSrcSpanFromPos startPos endPos) pName
  <?> "package declaration"

importDecl :: P ImportDecl
importDecl = do
    startPos <- getStartPos
    keyword KW_Import
    isStatic <- bopt $ keyword KW_Static
    pkgTypeName <- name ambigName <?> "package/type name"
    hasStar <- bopt $ dot >> (tok Op_Star <?> "* or identifier")
    semiColon
    endPos <- getEndPos
    return $ mkImportDecl isStatic hasStar (srcSpanToAnn $ mkSrcSpanFromPos startPos endPos) pkgTypeName
    <?> "import declaration"
  where mkImportDecl False False sp n = SingleTypeImport     sp (qualifiedTypeName $ flattenName n)
        mkImportDecl False True  sp n = TypeImportOnDemand   sp (pkgOrTypeName $ flattenName n)
        mkImportDecl True  False sp n = SingleStaticImport   sp (mkSingleStaticImport n)
        mkImportDecl True  True  sp n = StaticImportOnDemand sp (qualifiedTypeName $ flattenName n)

        mkSingleStaticImport n =
          let flName = flattenName n
              (lastI, initN) = (last flName, init flName)
          in Name (nameAnn n) lastI (nameType n) (if null initN
                                                    then Nothing
                                                    else (Just $ typeName initN))

typeDecl :: P (Maybe TypeDecl)
typeDecl = Just <$> classOrInterfaceDecl
       <|> const Nothing <$> semiColon
  <?> "type declaration"

classOrInterfaceDecl :: P TypeDecl
classOrInterfaceDecl = withModifiers $
  (do cdModsFun <- classDeclModsFun
      return $ \mods -> ClassTypeDecl (cdModsFun mods))
    <|>
  (do intdModsFun <- interfaceDeclModsFun
      return $ \mods -> InterfaceTypeDecl (intdModsFun mods))

classDeclModsFun :: P (ModifiersFun ClassDecl)
classDeclModsFun = normalClassDeclModsFun

normalClassDeclModsFun :: P (ModifiersFun ClassDecl)
normalClassDeclModsFun = do
  startPos <- getStartPos
  keyword KW_Class
  classId <- ident <?> "class name"
  body <- classBody
  endPos <- getEndPos
  return $ \mods ->
    let startPos' = getModifiersStartPos mods startPos
    in ClassDecl (srcSpanToAnn $ mkSrcSpanFromPos startPos' endPos) mods classId [] Nothing [] body

interfaceDeclModsFun :: P (ModifiersFun InterfaceDecl)
interfaceDeclModsFun = do
  startPos <- getStartPos
  keyword KW_Interface
  iId <- ident <?> "interface name"
  openCurly
  closeCurly
  endPos <- getEndPos
  return $ \mods ->
    let startPos' = getModifiersStartPos mods startPos
    in InterfaceDecl (srcSpanToAnn $ mkSrcSpanFromPos startPos' endPos) mods iId [] [] IB

classBody :: P ClassBody
classBody = do
  startPos <- getStartPos
  decls <- braces classBodyDecls
  endPos <- getEndPos
  return $ ClassBody (srcSpanToAnn $ mkSrcSpanFromPos startPos endPos) decls
  <?> "class body"

classBodyDecls :: P [ClassBodyDecl]
classBodyDecls = list classBodyDecl

classBodyDecl :: P (ClassBodyDecl)
classBodyDecl = withModifiers (do
  membDeclModsFun <- memberDeclModsFun
  return $ \mods -> MemberDecl (membDeclModsFun mods))
  <?> "class body declaration"

memberDeclModsFun :: P (ModifiersFun MemberDecl)
memberDeclModsFun = do
  startPos <- getStartPos
  retT <- returnType  -- parse the most general type
  case returnTypeToType retT of
    Just t  -> do
      -- member can be either field or method
      -- continue with identifier
      idStartPos <- getStartPos
      i <- ident <?> "field/method name"
      fieldDeclAfterTypeIdModsFun t startPos i idStartPos (varDecl "field") <|>
        methodDeclAfterTypeIdModsFun retT startPos i
    -- member can only be method (void or lock return type)
    Nothing -> methodDeclAfterTypeModsFun retT startPos

-- | Continues to parse field declaration after type and first identifier have been parsed.
fieldDeclAfterTypeIdModsFun :: Type -> SrcPos
                            -> Id -> SrcPos
                            -> P VarDecl
                            -> P (ModifiersFun MemberDecl)
fieldDeclAfterTypeIdModsFun t startPos varId varStartPos varDeclFun = do
  -- continue to parse first VarDecl
  vInit <- opt $ tok Op_Assign >> varInit
  varEndPos <- getEndPos
  let varD = VarDecl (srcSpanToAnn $ mkSrcSpanFromPos varStartPos varEndPos) varId vInit

  -- try other VarDecls
  hasComma <- bopt comma
  varDs <- if hasComma
             then varDecls varDeclFun
             else return []

  semiColon
  endPos <- getEndPos
  return $ \mods ->
    let startPos' = getModifiersStartPos mods startPos
    in FieldDecl (srcSpanToAnn $ mkSrcSpanFromPos startPos' endPos) mods t (varD:varDs)

-- | Continues to parse method declaration after return type has been parsed.
methodDeclAfterTypeModsFun :: ReturnType -> SrcPos -> P (ModifiersFun MemberDecl)
methodDeclAfterTypeModsFun retT startPos = do
  mId <- ident <?> "method name"
  methodDeclAfterTypeIdModsFun retT startPos mId

-- | Continues to parse method declaration after return type and method identifier have been parsed.
methodDeclAfterTypeIdModsFun :: ReturnType -> SrcPos -> Id -> P (ModifiersFun MemberDecl)
methodDeclAfterTypeIdModsFun retT startPos mId = do
  (formalPs, isValid) <- parens $ formalParams
  -- See comment on formalParams about failing
  when (not isValid) $ do
    -- Very, very, very ugly hack to get the desired position in the error message
    -- See http://haskell.1045720.n5.nabble.com/Parsec-Custom-Fail-td3131949.html
    setPosition (srcPosToParsec $ srcSpanToStartPos $
                 annSrcSpan $ getAnn $
                 fromJust $ find formalParamVarArity formalPs)  -- should not fail
    void anyToken
    fail "Only the last formal parameter may be of variable arity"
  body <- methodBody
  endPos <- getEndPos
  return $ \mods ->
    let startPos' = getModifiersStartPos mods startPos
    in MethodDecl (srcSpanToAnn $ mkSrcSpanFromPos startPos' endPos) mods [] retT mId formalPs body

-- | Takes 'VarDecl' parser to handle restrictions on field declarations in interfaces
-- (required presence of initializer).
varDecls :: P VarDecl -> P [VarDecl]
varDecls varDeclFun = varDeclFun `sepBy1` comma

varDecl :: String -> P VarDecl
varDecl desc = do
  startPos <- getStartPos
  varId <- ident <?> desc ++ " name"
  vInit <- opt $ tok Op_Assign >> varInit
  endPos <- getEndPos
  return $ VarDecl (srcSpanToAnn $ mkSrcSpanFromPos startPos endPos) varId vInit

-- | Parses formal parameters and returns them as a list together with validity
-- indicator (whether parameter of variable arity is the last one). This is
-- done in this way (returning a flag and not failing right here) for better
-- error message.
formalParams :: P ([FormalParam], Bool)
formalParams = do
    formalPs <- seplist formalParam comma
    return (formalPs, validateFormalParams formalPs)
  where validateFormalParams :: [FormalParam] -> Bool
        validateFormalParams []  = True
        validateFormalParams [_] = True
        validateFormalParams (FormalParam { formalParamVarArity = varArity } : fps) =
          not varArity && validateFormalParams fps

formalParam :: P (FormalParam)
formalParam = withModifiers (do
  startPos <- getStartPos
  paramType <- ttype <?> "formal parameter type"
  varArity <- bopt ellipsis
  paramId <- ident <?> "formal parameter name"
  endPos <- getEndPos
  return $ \mods ->
    let startPos' = getModifiersStartPos mods startPos
    in FormalParam (srcSpanToAnn $ mkSrcSpanFromPos startPos' endPos) [] paramType varArity paramId)
  <?> "formal parameter"

methodBody :: P MethodBody
methodBody = do
  startPos <- getStartPos
  mBlock <- const Nothing <$> semiColon
        <|> Just <$> block
  endPos <- getEndPos
  return $ MethodBody (srcSpanToAnn $ mkSrcSpanFromPos startPos endPos) mBlock

varInit :: P VarInit
varInit = InitExp <$> exp

block :: P Block
block = do
  startPos <- getStartPos
  blStmts <- braces $ list blockStmt
  endPos <- getEndPos
  return $ Block (srcSpanToAnn $ mkSrcSpanFromPos startPos endPos) blStmts

blockStmt :: P BlockStmt
blockStmt = do
  startPos <- getStartPos
  mods <- list modifier <?> "local variable declaration"  -- fixing error message
  if not (null mods)
    then localVarDecl startPos mods
    else try (localVarDecl startPos mods)
           <|>
         BlockStmt <$> stmt <?> "statement"

localVarDecl :: SrcPos -> [Modifier] -> P BlockStmt
localVarDecl startPos mods = do
  t <- ttype
  varDs <- varDecls (varDecl "variable")
  semiColon
  endPos <- getEndPos            
  return $ LocalVars (srcSpanToAnn $ mkSrcSpanFromPos startPos endPos) mods t varDs
  <?> "local variable declaration"

