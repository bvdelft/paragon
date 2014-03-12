module Language.Java.Paragon.NameResolutionSpec (main, spec) where

import Test.Hspec

import Language.Java.Paragon.NameResolution

import Control.Exception (tryJust)
import Control.Monad (guard)
import System.Environment (getEnv)
import System.FilePath ((</>), splitSearchPath)
import System.IO.Error (isDoesNotExistError)

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

successCase :: String -> AST SrcSpan -> IO ()
successCase fileName result = do
  input <- successRead fileName
  let (Right ast) = parse input fileName
  piPath <- getPIPATH
  (Right nrAst) <- runBaseM [] (liftToBaseM piPath (resolveNames ast))
  nrAst `shouldBe` result



-- | Main specification function.
spec :: Spec
spec = do
  describe "resolveNames" $ do
    -- Success
    it "resolves a class with empty body" $
      let fileName = "ClassDeclEmpty.para"
          srcSpanFun = SrcSpan fileName
          cdSrcSpan = srcSpanFun 1 1 1 10
          cbSrcSpan = srcSpanFun 1 9 1 10
          cd = ClassDecl cdSrcSpan [] cId [] Nothing [] (ClassBody cbSrcSpan [])
          cId = Id (srcSpanFun 1 7 1 7) "C"
      in  successCase fileName (CompilationUnit cdSrcSpan Nothing [] [ClassTypeDecl cd])
