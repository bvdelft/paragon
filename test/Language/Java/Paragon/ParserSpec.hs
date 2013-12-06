{-# OPTIONS_GHC
    -fno-warn-orphans
 #-}

-- | Parser tests.
module Language.Java.Paragon.ParserSpec (main, spec) where

import Test.Hspec
import Text.ParserCombinators.Parsec.Error

import Language.Java.Paragon.Syntax
import Language.Java.Paragon.Parser
import Language.Java.Paragon.SrcPos

-- | To be able to run this module from GHCi.
main :: IO ()
main = hspec spec

instance Eq ParseError where
  a == b = errorPos a == errorPos b &&
           errorMessages a == errorMessages b

instance Eq Message where
  (==) = messageEq

-- | Main specification function.
spec :: Spec
spec = do
  describe "parse" $ do
    -- Success
    it "parses an empty program" $
      let cuSrcSpan = SrcSpan "empty" 1 1 1 1 in
      parse "" "empty"
        `shouldBe`
      Right (CompilationUnit cuSrcSpan Nothing [] [])

    it "parses package declaration with single identifier" $
      let fileName = "PkgDecl"
          cuSrcSpan = SrcSpan fileName 1 1 1 16
          pdSrcSpan = SrcSpan fileName 1 1 1 16
          qIdSrcSpan = SrcSpan fileName 1 9 1 15
          paragonId = Id (SrcSpan fileName 1 9 1 15) "paragon" in
      parse "package paragon;" fileName
        `shouldBe`
      Right (CompilationUnit cuSrcSpan (Just $ PackageDecl pdSrcSpan (QId qIdSrcSpan paragonId PkgName Nothing)) [] [])

    it "parses package declaration with qualified identifier" $
      let fileName = "PkgDecl"
          cuSrcSpan = SrcSpan fileName 1 1 1 28
          pdSrcSpan = SrcSpan fileName 1 1 1 28
          qIdSrcSpan = SrcSpan fileName 1 9 1 27
          seId = Id (SrcSpan fileName 1 9 1 10) "se"
          seQId = QId (SrcSpan fileName 1 9 1 10) seId PkgName Nothing
          chalmersId = Id (SrcSpan fileName 1 12 1 19) "chalmers"
          chalmersQId = QId (SrcSpan fileName 1 9 1 19) chalmersId PkgName (Just seQId)
          paragonId = Id (SrcSpan fileName 1 21 1 27) "paragon" in
      parse "package se.chalmers.paragon;" fileName
        `shouldBe`
      Right (CompilationUnit cuSrcSpan (Just $ PackageDecl pdSrcSpan (QId qIdSrcSpan paragonId PkgName (Just chalmersQId))) [] [])

    -- Failure
    context "given a package declaration with missing semicolon" $
      it "gives an error message" $
        let Left err = parse "package paragon" "PkgDecl"
        in show err `shouldBe` "\"PkgDecl\" (line 1, column 9):\nunexpected end of input"

