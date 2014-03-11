module Language.Java.Paragon.NameResolutionSpec (main, spec) where

import Test.Hspec

import Language.Java.Paragon.NameResolution

import System.FilePath ((</>))

import Language.Java.Paragon.Monad.Base
import Language.Java.Paragon.Monad.PiReader
import Language.Java.Paragon.Syntax
import Language.Java.Paragon.Parser
import Language.Java.Paragon.SrcPos

-- | To be able to run this module from GHCi.
main :: IO ()
main = hspec spec

-- Configuration

piPath :: [String]
piPath = ["/scratch/paragon/paragonDARCS/lib"]

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
