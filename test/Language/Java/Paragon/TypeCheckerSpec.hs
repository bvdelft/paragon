module Language.Java.Paragon.TypeCheckerSpec
  (
    main
  , spec
  ) where

import Test.Hspec

import Language.Java.Paragon.ASTHelpers
import Language.Java.Paragon.Error
import Language.Java.Paragon.Error.StandardContexts
import Language.Java.Paragon.Interaction.IO
import Language.Java.Paragon.Monad.Base
import Language.Java.Paragon.Monad.PiReader
import Language.Java.Paragon.NameResolution
import Language.Java.Paragon.Syntax
import Language.Java.Paragon.Parser

import Language.Java.Paragon.TypeChecker
import Language.Java.Paragon.TypeChecker.Errors

import System.FilePath ((</>), takeBaseName)

-- | To be able to run this module from GHCi.
main :: IO ()
main = hspec spec

-- Configuration

testDir :: FilePath
testDir = "test" </> "typechecktests"

successDir :: FilePath
successDir = testDir </> "success"

failureDir :: FilePath
failureDir = testDir </> "failure"

-- Helpers for creating AST and checking

successRead :: String -> IO String
successRead fileName = readFile (successDir </> fileName)

failureRead :: String -> IO String
failureRead fileName = readFile (failureDir </> fileName)

{-
successCase :: String -> AST -> IO ()
successCase fileName result = do
  let baseName = takeBaseName fileName
  ast <- parseSuccessFile fileName
  piPath <- getPIPATH
  (Right nrAst) <- runBaseM [] (runPiReader piPath (resolveNames ast))
  (Right tcAst) <- runBaseM [] (typeCheck piPath baseName nrAst)
  tcAst `shouldBe` result
-}

noAltering :: String -> IO ()
noAltering fileName = do
  let baseName = takeBaseName fileName
  ast <- parseSuccessFile fileName
  piPath <- getPIPATH
  (Right (nrAst, tcAst)) <- 
      runBaseM [] $ runPiReader piPath $ do
                        nrAst <- resolveNames ast
                        tcAst <- typeCheck baseName nrAst
                        return (nrAst, tcAst)
  tcAst `shouldBe` nrAst

parseSuccessFile :: String -> IO AST
parseSuccessFile fileName = do
  input <- successRead fileName
  let (Right ast) = parse input fileName
  return ast

parseFailureFile :: String -> IO AST
parseFailureFile fileName = do
  input <- failureRead fileName
  let (Right ast) = parse input fileName
  return ast

failureCase :: String -> [Error] -> IO ()
failureCase fileName err = do
  let baseName = takeBaseName fileName
  ast <- parseFailureFile fileName
  piPath <- getPIPATH
  Left e <- runBaseM [] $ runPiReader piPath $ do
                             nrAst <- resolveNames ast
                             typeCheck baseName nrAst
  e `shouldBe` err

-- | Main specification function. Relies on successful parsing.
spec :: Spec
spec = do
  describe "typeCheck" $ do
  
    -- Success
    
    it "type checks an empty class declaration" $ do
      noAltering "ClassDeclEmpty.para"
    
    -- Failure
    
    it "detects that the declared class does not match the file name" $ do
      let ctxt      = defClassBodyContext "C"
          fileName  = "FileNameMismatch.para"
          cSrcSpan  = makeSrcSpanAnn fileName 1 14 1 14
          cId       = Id cSrcSpan "C"
          classDecl = (defClassDecl "C") { cdId = cId}
          err       = fileNameMismatch "FileNameMismatch" classDecl cId [tcCtxt, ctxt]
      failureCase fileName [err]

tcCtxt :: ErrorContext
tcCtxt = compPhaseContext "Type Checking"
