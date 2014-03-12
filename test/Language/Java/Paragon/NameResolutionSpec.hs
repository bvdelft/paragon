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

-- Helpers

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
          
    -- Success, resolving does alter AST
    
    it "parses class declaration with single field declaration with reference type" $ do      
      ast <- parseFile "ClassDeclSingleFieldRefType.para"
      -- construct new name prefix
      let newPrefix = mkName (\x -> \_ -> x) PkgName [Id defaultSpan "java", Id defaultSpan "lang"]
      -- replace prefix in ast
      -- TODO: find an easier way :D
      let (ClassTypeDecl typeDecl) = head (cuTypeDecls ast)
      let (MemberDecl fieldDecl)   = head (cbDecls (cdBody typeDecl))
      let (RefType (ClassRefType crt)) = fieldDeclType fieldDecl
      let newAst = ast { cuTypeDecls = [ClassTypeDecl $ typeDecl {
                         cdBody = (cdBody typeDecl) {
                           cbDecls = [MemberDecl $ fieldDecl {
                             fieldDeclType = RefType (ClassRefType (crt {
                               ctName = (ctName crt) {
                                   namePrefix = Just newPrefix
                                 }
                               }))
                             }]
                           }
                         }]
                       }
      afterAltering ast newAst
{-
          srcSpanFun = SrcSpan fileName
          cdSrcSpan = srcSpanFun 1 1 3 1
          cbSrcSpan = srcSpanFun 1 9 3 1
          fdSrcSpan = srcSpanFun 2 3 2 11
          cbDecl = MemberDecl fieldDecl
          tSrcSpan = srcSpanFun 2 3 2 8
          varId = Id varSrcSpan "x"
          varSrcSpan = srcSpanFun 2 10 2 10
          varDecl = VarDecl varSrcSpan varId Nothing
          tName = Name tSrcSpan (Id tSrcSpan "Object") TypeName Nothing
          fieldDecl = FieldDecl fdSrcSpan [] (RefType (ClassRefType (ClassType tSrcSpan tName []))) [varDecl]
          cd = ClassDecl cdSrcSpan [] cId [] Nothing [] (ClassBody cbSrcSpan [cbDecl])
          cId = Id (srcSpanFun 1 7 1 7) "C"
      in successCase fileName (CompilationUnit cdSrcSpan Nothing [] [ClassTypeDecl cd])

    it "parses class declaration with single field declaration of primitive type with literal initializer" $
      let fileName = "ClassDeclSinglePrimFieldInit.para"
          srcSpanFun = SrcSpan fileName
          cdSrcSpan = srcSpanFun 1 1 3 1
          cbSrcSpan = srcSpanFun 1 9 3 1
          fdSrcSpan = srcSpanFun 2 3 2 19
          cbDecl = MemberDecl fieldDecl
          tSrcSpan = srcSpanFun 2 3 2 9
          fieldDecl = FieldDecl fdSrcSpan [] (PrimType (BooleanT tSrcSpan)) [varDecl]
          varId = Id varSrcSpan "b"
          varSrcSpan = srcSpanFun 2 11 2 11
          eSrcSpan = srcSpanFun 2 15 2 18
          varDecl = VarDecl (srcSpanFun 2 11 2 18) varId (Just $ InitExp (Lit (Boolean eSrcSpan True)))
          cd = ClassDecl cdSrcSpan [] cId [] Nothing [] (ClassBody cbSrcSpan [cbDecl])
          cId = Id (srcSpanFun 1 7 1 7) "C"
      in successCase fileName (CompilationUnit cdSrcSpan Nothing [] [ClassTypeDecl cd])

    it "parses class declaration with single field declaration of reference type with null initializer" $
      let fileName = "ClassDeclSingleRefFieldInit.para"
          srcSpanFun = SrcSpan fileName
          cdSrcSpan = srcSpanFun 1 1 3 1
          cbSrcSpan = srcSpanFun 1 9 3 1
          fdSrcSpan = srcSpanFun 2 3 2 18
          cbDecl = MemberDecl fieldDecl
          tSrcSpan = srcSpanFun 2 3 2 8
          tName = Name tSrcSpan (Id tSrcSpan "Object") TypeName Nothing
          fieldDecl = FieldDecl fdSrcSpan [] (RefType (ClassRefType (ClassType tSrcSpan tName []))) [varDecl]
          varId = Id varSrcSpan "x"
          varSrcSpan = srcSpanFun 2 10 2 10
          eSrcSpan = srcSpanFun 2 14 2 17
          varDecl = VarDecl (srcSpanFun 2 10 2 17) varId (Just $ InitExp (Lit (Null eSrcSpan)))
          cd = ClassDecl cdSrcSpan [] cId [] Nothing [] (ClassBody cbSrcSpan [cbDecl])
          cId = Id (srcSpanFun 1 7 1 7) "C"
      in successCase fileName (CompilationUnit cdSrcSpan Nothing [] [ClassTypeDecl cd])

    it "parses class declaration with multiple field declarations of primitive types with literal initializers" $
      let fileName = "ClassDeclMultPrimFieldsInit.para"
          srcSpanFun = SrcSpan fileName
          cdSrcSpan = srcSpanFun 1 1 3 1
          cbSrcSpan = srcSpanFun 1 9 3 1
          fdSrcSpan = srcSpanFun 2 3 2 19
          cbDecl = MemberDecl fieldDecl
          tSrcSpan = srcSpanFun 2 3 2 5
          fieldDecl = FieldDecl fdSrcSpan [] (PrimType (IntT tSrcSpan)) [varDecl1, varDecl2]
          varId1 = Id varSrcSpan1 "x"
          varSrcSpan1 = srcSpanFun 2 7 2 7
          eSrcSpan1 = srcSpanFun 2 11 2 11
          varDecl1 = VarDecl (srcSpanFun 2 7 2 11) varId1 (Just $ InitExp (Lit (Int eSrcSpan1 1)))
          varId2 = Id varSrcSpan2 "y"
          varSrcSpan2 = srcSpanFun 2 14 2 14
          eSrcSpan2 = srcSpanFun 2 18 2 18
          varDecl2 = VarDecl (srcSpanFun 2 14 2 18) varId2 (Just $ InitExp (Lit (Int eSrcSpan2 2)))
          cd = ClassDecl cdSrcSpan [] cId [] Nothing [] (ClassBody cbSrcSpan [cbDecl])
          cId = Id (srcSpanFun 1 7 1 7) "C"
      in successCase fileName (CompilationUnit cdSrcSpan Nothing [] [ClassTypeDecl cd])

    it "parses class declaration with void method with single local variable declaration of reference type with null initializer" $
      let fileName = "ClassDeclVoidMethodSingleRefLocalVarInit.para"
          srcSpanFun = SrcSpan fileName
          cdSrcSpan = srcSpanFun 1 1 5 1
          cbSrcSpan = srcSpanFun 1 9 5 1
          mdSrcSpan = srcSpanFun 2 3 4 3
          cbDecl = MemberDecl methodDecl
          mId = Id (srcSpanFun 2 8 2 8) "f"
          bodySrcSpan = srcSpanFun 2 12 4 3
          stmtSrcSpan = srcSpanFun 3 5 3 20
          tSrcSpan = srcSpanFun 3 5 3 10
          tName = Name tSrcSpan (Id tSrcSpan "Object") TypeName Nothing
          objectT = RefType (ClassRefType (ClassType tSrcSpan tName []))
          varId = Id varSrcSpan "x"
          varSrcSpan = srcSpanFun 3 12 3 12
          eSrcSpan = srcSpanFun 3 16 3 19
          varDecl = VarDecl (srcSpanFun 3 12 3 19) varId (Just $ InitExp (Lit (Null eSrcSpan)))
          body = MethodBody bodySrcSpan (Just (Block bodySrcSpan [LocalVars stmtSrcSpan [] objectT [varDecl]]))
          methodDecl = MethodDecl mdSrcSpan [] [] (VoidType (srcSpanFun 2 3 2 6)) mId [] body
          cd = ClassDecl cdSrcSpan [] cId [] Nothing [] (ClassBody cbSrcSpan [cbDecl])
          cId = Id (srcSpanFun 1 7 1 7) "C"
      in successCase fileName (CompilationUnit cdSrcSpan Nothing [] [ClassTypeDecl cd])

    it "parses class declaration with single low policy field" $
      let fileName = "ClassDeclSingleLowPolicyField.para"
          srcSpanFun = SrcSpan fileName
          cdSrcSpan = srcSpanFun 1 1 3 1
          cbSrcSpan = srcSpanFun 1 9 3 1
          fdSrcSpan = srcSpanFun 2 3 2 43
          cbDecl = MemberDecl fieldDecl
          tSrcSpan = srcSpanFun 2 16 2 21
          mods = [Public $ srcSpanFun 2 3 2 8, Final $ srcSpanFun 2 10 2 14]
          fieldDecl = FieldDecl fdSrcSpan mods (PrimType (PolicyT tSrcSpan)) [varDecl]
          varId = Id varSrcSpan "low"
          varSrcSpan = srcSpanFun 2 23 2 25
          eSrcSpan = srcSpanFun 2 29 2 42
          cl = Clause (srcSpanFun 2 31 2 40) [] clHead []
          clHead = ClauseDeclHead clVarDecl
          clVarTSrcSpan = srcSpanFun 2 31 2 36
          clVarTName = Name clVarTSrcSpan (Id clVarTSrcSpan "Object") TypeName Nothing
          clVarType = ClassRefType (ClassType clVarTSrcSpan clVarTName [])
          clVarDeclSrcSpan = srcSpanFun 2 31 2 38
          clVarDecl = ClauseVarDecl clVarDeclSrcSpan clVarType (Id (srcSpanFun 2 38 2 38) "x")
          policyE = PolicyExp (PolicyLit eSrcSpan [cl])
          varDecl = VarDecl (srcSpanFun 2 23 2 42) varId (Just $ InitExp policyE)
          cd = ClassDecl cdSrcSpan [] cId [] Nothing [] (ClassBody cbSrcSpan [cbDecl])
          cId = Id (srcSpanFun 1 7 1 7) "C"
      in successCase fileName (CompilationUnit cdSrcSpan Nothing [] [ClassTypeDecl cd])

    it "parses class declaration with single high policy field" $
      let fileName = "ClassDeclSingleHighPolicyField.para"
          srcSpanFun = SrcSpan fileName
          cdSrcSpan = srcSpanFun 1 1 3 1
          cbSrcSpan = srcSpanFun 1 9 3 1
          fdSrcSpan = srcSpanFun 2 3 2 33
          cbDecl = MemberDecl fieldDecl
          tSrcSpan = srcSpanFun 2 16 2 21
          mods = [Public $ srcSpanFun 2 3 2 8, Final $ srcSpanFun 2 10 2 14]
          fieldDecl = FieldDecl fdSrcSpan mods (PrimType (PolicyT tSrcSpan)) [varDecl]
          varId = Id varSrcSpan "high"
          varSrcSpan = srcSpanFun 2 23 2 26
          eSrcSpan = srcSpanFun 2 30 2 32
          policyE = PolicyExp (PolicyLit eSrcSpan [])
          varDecl = VarDecl (srcSpanFun 2 23 2 32) varId (Just $ InitExp policyE)
          cd = ClassDecl cdSrcSpan [] cId [] Nothing [] (ClassBody cbSrcSpan [cbDecl])
          cId = Id (srcSpanFun 1 7 1 7) "C"
      in successCase fileName (CompilationUnit cdSrcSpan Nothing [] [ClassTypeDecl cd])

    it "parses class declaration with single field with reads policy modifier" $
      let fileName = "ClassDeclSingleFieldReadsMod.para"
          srcSpanFun = SrcSpan fileName
          cdSrcSpan = srcSpanFun 1 1 3 1
          cbSrcSpan = srcSpanFun 1 9 3 1
          fdSrcSpan = srcSpanFun 2 3 2 13
          cbDecl = MemberDecl fieldDecl
          tSrcSpan = srcSpanFun 2 8 2 10
          mods = [Reads (srcSpanFun 2 3 2 6) p]
          pSrcSpan = srcSpanFun 2 4 2 6
          p = PolicyExp (PolicyLit pSrcSpan [])
          fieldDecl = FieldDecl fdSrcSpan mods (PrimType (IntT tSrcSpan)) [varDecl]
          varId = Id varSrcSpan "x"
          varSrcSpan = srcSpanFun 2 12 2 12
          varDecl = VarDecl varSrcSpan varId Nothing
          cd = ClassDecl cdSrcSpan [] cId [] Nothing [] (ClassBody cbSrcSpan [cbDecl])
          cId = Id (srcSpanFun 1 7 1 7) "C"
      in successCase fileName (CompilationUnit cdSrcSpan Nothing [] [ClassTypeDecl cd])

    it "parses class declaration with single field with writes policy modifier" $
      let fileName = "ClassDeclSingleFieldWritesMod.para"
          srcSpanFun = SrcSpan fileName
          cdSrcSpan = srcSpanFun 1 1 3 1
          cbSrcSpan = srcSpanFun 1 9 3 1
          fdSrcSpan = srcSpanFun 2 3 2 13
          cbDecl = MemberDecl fieldDecl
          tSrcSpan = srcSpanFun 2 8 2 10
          mods = [Writes (srcSpanFun 2 3 2 6) p]
          pSrcSpan = srcSpanFun 2 4 2 6
          p = PolicyExp (PolicyLit pSrcSpan [])
          fieldDecl = FieldDecl fdSrcSpan mods (PrimType (IntT tSrcSpan)) [varDecl]
          varId = Id varSrcSpan "x"
          varSrcSpan = srcSpanFun 2 12 2 12
          varDecl = VarDecl varSrcSpan varId Nothing
          cd = ClassDecl cdSrcSpan [] cId [] Nothing [] (ClassBody cbSrcSpan [cbDecl])
          cId = Id (srcSpanFun 1 7 1 7) "C"
      in successCase fileName (CompilationUnit cdSrcSpan Nothing [] [ClassTypeDecl cd])
-}
{- Fail -- and supposed to do so
    it "resolves an empty program" $ noAltering "Empty.para"
    it "parses class declaration with void method with single assignment expression statement with literal" $
      noAltering "ClassDeclVoidMethodSingleAssignLit.para"
    it "parses class declaration with void method with single assignment expression statement with variable" $
      noAltering "ClassDeclVoidMethodSingleAssignVar.para"
-}
