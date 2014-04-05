module Language.Java.Paragon.NameResolutionSpec
  (
    main
  , spec
  , getFailing
  ) where

import Test.Hspec

import Language.Java.Paragon.NameResolution
import Language.Java.Paragon.NameResolution.Errors

import Control.Exception (tryJust)
import Control.Monad (guard)
import System.Environment (getEnv)
import System.FilePath ((</>), (<.>), splitSearchPath)
import System.IO.Error (isDoesNotExistError)

import Language.Java.Paragon.Annotation
import Language.Java.Paragon.ASTHelpers
import Language.Java.Paragon.Error
import Language.Java.Paragon.Error.StandardContexts
import Language.Java.Paragon.Error.StandardErrors
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

failureDir :: FilePath
failureDir = testDir </> "failure"

-- Helpers for creating AST and checking

successRead :: String -> IO String
successRead baseName = readFile (successDir </> mkFileName baseName)

failureRead :: String -> IO String
failureRead baseName = readFile (failureDir </> mkFileName baseName)

successCase :: AST -> AST -> IO ()
successCase ast result = do
  piPath <- getPIPATH
  (Right nrAst) <- runBaseM [] (liftToBaseM piPath (resolveNames ast))
  nrAst `shouldBe` result

parseSuccessFile :: String -> IO AST
parseSuccessFile baseName = do
  input <- successRead baseName
  let (Right ast) = parse input (mkFileName baseName)
  return ast

parseFailureFile :: String -> IO AST
parseFailureFile baseName = do
  input <- failureRead baseName
  let (Right ast) = parse input (mkFileName baseName)
  return ast

noAltering :: String -> IO ()
noAltering baseName = do
  ast <- parseSuccessFile baseName
  successCase ast ast

failureCase :: String -> [Error] -> IO ()
failureCase baseName err = do
  ast <- parseFailureFile baseName
  piPath <- getPIPATH
  (Left e) <- runBaseM [] (liftToBaseM piPath (resolveNames ast))
  e `shouldBe` err

mkFileName :: String -> String
mkFileName baseName = baseName <.> "para"

-- For debugging when a test fails

getFailing :: String -> IO [Error]
getFailing file = do
  ast <- parseFailureFile file
  piPath <- getPIPATH
  (Left err) <- runBaseM [] (liftToBaseM piPath (resolveNames ast))
  return err

-- | Main specification function. Relies on successful parsing.
spec :: Spec
spec = do
  describe "resolveNames" $ do

    -- Success, resolving does not alter AST

    it "resolves empty class declaration" $
      noAltering "ClassDeclEmpty"
    it "resolves empty class declaration with modifiers" $
      noAltering "ClassDeclMod"
    it "resolves class declaration with single field declaration" $
      noAltering "ClassDeclSingleField"
    it "resolves class declaration with single field declaration with modifiers" $
      noAltering "ClassDeclSingleFieldMod"
    it "resolves class declaration with multiple field declarations with modifiers" $
      noAltering "ClassDeclMultFields"
    it "resolves class declaration with void method with semicolon body" $
      noAltering "ClassDeclVoidMethodSemiColon"
    it "resolves class declaration with int method with semicolon body" $
      noAltering "ClassDeclIntMethodSemiColon"
    it "resolves class declaration with void method with empty body" $
      noAltering "ClassDeclVoidMethodEmptyBody"
    it "resolves class declaration with int method with empty body with modifiers" $
      noAltering "ClassDeclIntMethodModEmptyBody"
    it "resolves class declaration with void method with empty statement" $
      noAltering "ClassDeclVoidMethodSemiColonStmt"
    it "resolves class declaration with void method with single local variable declaration with modifier" $
      noAltering "ClassDeclVoidMethodSingleLocalVarDeclMod"
    it "resolves class declaration with void method with multiple local variable declarations" $
      noAltering "ClassDeclVoidMethodMultLocalVarDecls"
    it "resolves class declaration with single field declaration of primitive type with literal initializer" $
      noAltering "ClassDeclSinglePrimFieldInit"
    it "resolves class declaration with multiple field declarations of primitive types with literal initializers" $
      noAltering "ClassDeclMultPrimFieldsInit"
    it "parses class declaration with single high policy field" $
      noAltering "ClassDeclSingleHighPolicyField"
    it "parses class declaration with single field with reads policy modifier" $
      noAltering "ClassDeclSingleFieldReadsMod"
    it "parses class declaration with single field with writes policy modifier" $
      noAltering "ClassDeclSingleFieldWritesMod"

    -- Success, resolving does alter AST

    it "resolves class declaration with single field declaration with reference type" $ do
      ast <- parseSuccessFile "ClassDeclSingleFieldRefType"
      let newPrefix = mkName const PkgName [Id defaultAnn "java", Id defaultAnn "lang"]
      let transform = modifyBodyDecl 0 $ modifyFieldDeclType $
                        modifyRefTypePrefix (const $ Just newPrefix)
      successCase ast (transform ast)
    it "resolves class declaration with single field declaration of reference type with null initializer" $ do
      ast <- parseSuccessFile "ClassDeclSingleRefFieldInit"
      let newPrefix = mkName const PkgName [Id defaultAnn "java", Id defaultAnn "lang"]
      let transform = modifyBodyDecl 0 $ modifyFieldDeclType $
                        modifyRefTypePrefix (const $ Just newPrefix)
      successCase ast (transform ast)
    it "resolves class declaration with void method with single local variable declaration of reference type with null initializer" $ do
      ast <- parseSuccessFile "ClassDeclVoidMethodSingleRefLocalVarInit"
      let newPrefix = mkName const PkgName [Id defaultAnn "java", Id defaultAnn "lang"]
      let transform = modifyBodyDecl 0 $ modifyMethodBlockStmt 0 $
                        modifyLocalVarsType $ modifyRefTypePrefix (const $ Just newPrefix)
      successCase ast (transform ast)
    it "resolves class declaration with single low policy field" $ do
      ast <- parseSuccessFile "ClassDeclSingleLowPolicyField"
      let newPrefix = mkName const PkgName [Id defaultAnn "java", Id defaultAnn "lang"]
      let transform = modifyBodyDecl 0 $ modifyFieldDeclInitExp 0 $
                        modifyPolicyClause 0 $ modifyDeclHeadRef $
                          modifyRefTypePrefix (const $ Just newPrefix)
      successCase ast (transform ast)
    it "parses class declaration with shadowing reference type - reference type" $ do
      ast <- parseSuccessFile "ClassDeclShadowingRefRef"
      let t1 = modifyBodyDecl 0 $ modifyFieldDeclType $
                 modifyRefTypePrefix $ prefixNameType PkgName
      let t2 = modifyBodyDecl 1 $ modifyFieldDeclInitExp 0 $ modifyPolicyClause 0 $
                 modifyDeclHeadRef $ modifyRefTypePrefix $ prefixNameType PkgName
      let t3 = modifyBodyDecl 2 $ modifyMethodBlockStmt 0 $ modifyLocalVarsType $
                 modifyRefTypePrefix $ prefixNameType PkgName
      let transform = t3 . t2 . t1
      successCase ast (transform ast)
    it "parses class declaration with shadowing primitive type - reference type" $ do
      ast <- parseSuccessFile "ClassDeclShadowingPrimRef"
      let t2 = modifyBodyDecl 1 $ modifyFieldDeclInitExp 0 $ modifyPolicyClause 0 $
                 modifyDeclHeadRef $ modifyRefTypePrefix $ prefixNameType PkgName
      let t3 = modifyBodyDecl 2 $ modifyMethodBlockStmt 0 $ modifyLocalVarsType $
                 modifyRefTypePrefix $ prefixNameType PkgName
      let transform = t3 . t2
      successCase ast (transform ast)

    -- Failure, error should be as expected.

    it "cannot resolve an empty program" $ do
      let err = unsupportedError "compilation unit without type definition"
                  errorAnnotation [nrCtxt]
      failureCase "Empty" [err]
    it "refuses assignments to undefined variables" $ do
      let baseName = "ClassDeclVoidMethodSingleAssignLit"
          fileName = mkFileName baseName
          vSrcSpan = makeSrcSpanAnn fileName 3 5 3 5
          vId      = Id vSrcSpan "x"
          vName    = Name vSrcSpan vId ExpName Nothing
          ctxt     = [nrCtxt,defClassBodyContext "C",defMethodContext "f"]
          err      = unresolvedName vName vName ctxt
      failureCase baseName [err]
    it "cannot resolve an undefined variable on right-hand side" $ do
      let baseName = "ClassDeclVoidMethodSingleAssignVar"
          fileName = mkFileName baseName
          vSrcSpan = makeSrcSpanAnn fileName 4 9 4 9
          vId      = Id vSrcSpan "y"
          vName    = Name vSrcSpan vId ExpOrLockName Nothing
          ctxt     = [nrCtxt,defClassBodyContext "C",defMethodContext "f"]
          err      = unresolvedName vName vName ctxt
      failureCase baseName [err]

prefixNameType :: NameType -> Maybe Name -> Maybe Name
prefixNameType _ Nothing     = Nothing
prefixNameType t (Just name) =
  Just $ name { nameType   = t
              , namePrefix = prefixNameType t (namePrefix name) }

defaultAnn :: Annotation
defaultAnn = emptyAnnotation { annSrcSpan = defaultSpan }

makeSrcSpanAnn :: String -> Int -> Int -> Int -> Int -> Annotation
makeSrcSpanAnn fileName a b c d = srcSpanToAnn $ SrcSpan fileName a b c d

nrCtxt :: ErrorContext
nrCtxt = compPhaseContext "Name Resolution"
