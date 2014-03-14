module Language.Java.Paragon.NameResolutionSpec
  (
    main
  , spec
  , getFailing
  ) where

import Test.Hspec

import Language.Java.Paragon.NameResolution

import Control.Exception (tryJust)
import Control.Monad (guard)
import System.Environment (getEnv)
import System.FilePath ((</>), splitSearchPath)
import System.IO.Error (isDoesNotExistError)

import Language.Java.Paragon.Error
import Language.Java.Paragon.Monad.Base
import Language.Java.Paragon.Monad.PiReader
import Language.Java.Paragon.Syntax
import Language.Java.Paragon.Parser
import Language.Java.Paragon.SrcPos

-- | To be able to run this module from GHCi.
main :: IO ()
main = hspec spec

-- Configuration

getPIPATH :: IO [String]
getPIPATH = do
  -- guard indicates that the only expected exception is isDoesNotExistError
  -- returns an Either type, left exception, right path
  ePpStr <- tryJust (guard . isDoesNotExistError) $ getEnv "PIPATH"
  -- splitSearchPath comes from System.FilePath, splitting String into filepaths
  -- In case the PIPATH variable did not exist, the empty list is used.
  -- (either takes two functions, const makes a function ignoring the other
  -- argument, i.e. the exception is ignored).
  return $ splitSearchPath $ either (const []) id ePpStr

testDir :: FilePath
testDir = "test" </> "namerestests"

successDir :: FilePath
successDir = testDir </> "success"

{-
failureDir :: FilePath
failureDir = testDir </> "failure"
-}

-- Helpers for creating AST and checking

successRead :: String -> IO String
successRead fileName = readFile (successDir </> fileName)

successCase :: AST SrcSpan -> AST SrcSpan -> IO ()
successCase ast result = do
  piPath <- getPIPATH
  (Right nrAst) <- runBaseM [] (liftToBaseM piPath (resolveNames ast))
  nrAst `shouldBe` result

parseFile :: String -> IO (AST SrcSpan)
parseFile fileName = do
  input <- successRead fileName
  let (Right ast) = parse input fileName
  return ast

noAltering :: String -> IO ()
noAltering fileName = do
  ast <- parseFile fileName
  successCase ast ast

afterAltering :: AST SrcSpan -> AST SrcSpan -> IO ()
afterAltering ast targetAst = do
  piPath <- getPIPATH
  (Right nrAst) <- runBaseM [] (liftToBaseM piPath (resolveNames ast))
  nrAst `shouldBe` targetAst

-- For debugging when a test fails

getFailing :: String -> IO [Error]
getFailing file = do
  ast <- parseFile file
  piPath <- getPIPATH
  (Left err) <- runBaseM [] (liftToBaseM piPath (resolveNames ast))
  return err  

-- | Main specification function. Relies on successful parsing.
spec :: Spec
spec = do
  describe "resolveNames" $ do
  
    -- Success, resolving does not alter AST
    
    it "resolves empty class declaration" $ 
      noAltering "ClassDeclEmpty.para"
    it "resolves empty class declaration with modifiers" $
      noAltering "ClassDeclMod.para"
    it "resolves class declaration with single field declaration" $
      noAltering "ClassDeclSingleField.para"
    it "resolves class declaration with single field declaration with modifiers" $ 
      noAltering "ClassDeclSingleFieldMod.para"
    it "resolves class declaration with multiple field declarations with modifiers" $
      noAltering "ClassDeclMultFields.para"
    it "resolves class declaration with void method with semicolon body" $
      noAltering "ClassDeclVoidMethodSemiColon.para"
    it "resolves class declaration with int method with semicolon body" $
      noAltering "ClassDeclIntMethodSemiColon.para"
    it "resolves class declaration with void method with empty body" $
      noAltering "ClassDeclVoidMethodEmptyBody.para"
    it "resolves class declaration with int method with empty body with modifiers" $
      noAltering "ClassDeclIntMethodModEmptyBody.para"
    it "resolves class declaration with void method with empty statement" $
      noAltering "ClassDeclVoidMethodSemiColonStmt.para"
    it "resolves class declaration with void method with single local variable declaration with modifier" $
      noAltering "ClassDeclVoidMethodSingleLocalVarDeclMod.para"
    it "resolves class declaration with void method with multiple local variable declarations" $
      noAltering "ClassDeclVoidMethodMultLocalVarDecls.para"
    it "resolves class declaration with single field declaration of primitive type with literal initializer" $
      noAltering "ClassDeclSinglePrimFieldInit.para"
    it "resolves class declaration with multiple field declarations of primitive types with literal initializers" $
      noAltering "ClassDeclMultPrimFieldsInit.para"
    it "parses class declaration with single high policy field" $
      noAltering "ClassDeclSingleHighPolicyField.para"
    it "parses class declaration with single field with reads policy modifier" $
      noAltering "ClassDeclSingleFieldReadsMod.para"
    it "parses class declaration with single field with writes policy modifier" $
      noAltering "ClassDeclSingleFieldWritesMod.para"
          
    -- Success, resolving does alter AST
    
    it "resolves class declaration with single field declaration with reference type" $ do      
      ast <- parseFile "ClassDeclSingleFieldRefType.para"
      let newPrefix = mkName const PkgName [Id defaultSpan "java", Id defaultSpan "lang"]
      let transform = modifyBodyDecl 0 $ modifyFieldDeclType $ 
                        modifyRefTypePrefix (const $ Just newPrefix)
      afterAltering ast (transform ast)
    it "resolves class declaration with single field declaration of reference type with null initializer" $ do      
      ast <- parseFile "ClassDeclSingleRefFieldInit.para"
      let newPrefix = mkName const PkgName [Id defaultSpan "java", Id defaultSpan "lang"]
      let transform = modifyBodyDecl 0 $ modifyFieldDeclType $ 
                        modifyRefTypePrefix (const $ Just newPrefix)
      afterAltering ast (transform ast)
    it "resolves class declaration with void method with single local variable declaration of reference type with null initializer" $ do
      ast <- parseFile "ClassDeclVoidMethodSingleRefLocalVarInit.para"
      let newPrefix = mkName const PkgName [Id defaultSpan "java", Id defaultSpan "lang"]
      let transform = modifyBodyDecl 0 $ modifyMethodBlockStmt 0 $
                        modifyLocalVarsType $ modifyRefTypePrefix (const $ Just newPrefix)
      afterAltering ast (transform ast)
    it "resolves class declaration with single low policy field" $ do
      ast <- parseFile "ClassDeclSingleLowPolicyField.para"
      let newPrefix = mkName const PkgName [Id defaultSpan "java", Id defaultSpan "lang"]
      let transform = modifyBodyDecl 0 $ modifyFieldDeclInitExp 0 $
                        modifyPolicyClause 0 $ modifyDeclHeadRef $
                          modifyRefTypePrefix (const $ Just newPrefix)
      afterAltering ast (transform ast)

-- Helpers for modifying AST. Some assumption here, e.g. only altering RefType,
-- not PrimType.

type ASTModifier a b = (a -> a) -> b -> b

-- | Modify the i-th declaration in the body. Assumes there is only one
-- classTypeDecl.
modifyBodyDecl :: Int -> ASTModifier (ClassBodyDecl a) (AST a)
modifyBodyDecl i f ast =
  let (ClassTypeDecl classDecl) = head $ cuTypeDecls ast
      classBody = cdBody classDecl
      (xs,y:ys) = splitAt i (cbDecls classBody)
      newDecls  = xs ++ (f y:ys)
      newBody   = classBody { cbDecls = newDecls }
  in ast { cuTypeDecls = [ClassTypeDecl classDecl { cdBody = newBody} ] }

modifyRefTypePrefix :: ASTModifier (Maybe (Name a)) (RefType a)
modifyRefTypePrefix f (ClassRefType ct) =
  let name    = ctName ct
      newName = name { namePrefix = f (namePrefix name) }
  in ClassRefType ct { ctName = newName }

modifyFieldDeclType :: ASTModifier (RefType a) (ClassBodyDecl a)
modifyFieldDeclType f (MemberDecl fieldDecl) = 
  let (RefType rt) = fieldDeclType fieldDecl
      rt' = f rt
  in MemberDecl fieldDecl { fieldDeclType = RefType rt' }

modifyLocalVarsType :: ASTModifier (RefType a) (BlockStmt a)
modifyLocalVarsType f localVars =
  let (RefType rt) = localVarsType localVars
  in localVars { localVarsType = RefType $ f rt }

-- | Modify the i-th statement in this method
modifyMethodBlockStmt :: Int -> ASTModifier (BlockStmt a) (ClassBodyDecl a)
modifyMethodBlockStmt i f (MemberDecl methodDecl) =
  let body = methodDeclBody methodDecl
      bodyBlock = methodBodyBlock body
  in case bodyBlock of
       Just block -> let (xs,y:ys) = splitAt i (blockAnnStmts block)
                         newBlock  = block { blockAnnStmts = xs ++ (f y:ys) }
                     in (MemberDecl methodDecl { methodDeclBody =
                           body { methodBodyBlock = Just newBlock } } )
       Nothing    -> (MemberDecl methodDecl) 

modifyFieldDeclInitExp :: Int -> ASTModifier (Exp a) (ClassBodyDecl a)
modifyFieldDeclInitExp i f (MemberDecl fieldDecl) = 
  let (xs,y:ys) = splitAt i (fieldDeclVarDecls fieldDecl)
      newInit   = fmap (\x -> x { varInitExp = f (varInitExp x) }) (varDeclInit y)
      newVarD   = xs ++ (y { varDeclInit = newInit }:ys)
  in MemberDecl fieldDecl { fieldDeclVarDecls = newVarD }

modifyPolicyClause :: Int -> ASTModifier (Clause a) (Exp a)
modifyPolicyClause i f (PolicyExp polExp) =
  let (xs,y:ys) = splitAt i (policyClauses polExp)
  in PolicyExp polExp { policyClauses = xs ++ (f y:ys) }
modifyPolicyClause _ _ _ = error "Incorrect call by test: modifyPolicyClause"

modifyDeclHeadRef :: ASTModifier (RefType a) (Clause a)
modifyDeclHeadRef f clause =
  let (ClauseDeclHead hd) = clauseHead clause
      nt = f (clauseVarDeclType hd)
  in clause { clauseHead = ClauseDeclHead hd { clauseVarDeclType = nt } }

{- Fail -- and supposed to do so
    it "resolves an empty program" $ noAltering "Empty.para"
    it "parses class declaration with void method with single assignment expression statement with literal" $
      noAltering "ClassDeclVoidMethodSingleAssignLit.para"
    it "parses class declaration with void method with single assignment expression statement with variable" $
      noAltering "ClassDeclVoidMethodSingleAssignVar.para"
-}
