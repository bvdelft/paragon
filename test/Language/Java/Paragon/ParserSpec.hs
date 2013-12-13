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
    -- TODO: fix name types
    it "parses single type import declaration" $
      let fileName = "ImportDecl"
          cuSrcSpan = SrcSpan fileName 1 1 1 22
          impdSrcSpan = SrcSpan fileName 1 1 1 22
          qIdSrcSpan = SrcSpan fileName 1 8 1 21
          policyId = Id (SrcSpan fileName 1 16 1 21) "Policy"
          paragonQId = QId (SrcSpan fileName 1 8 1 14) paragonId AmbigName Nothing
          paragonId = Id (SrcSpan fileName 1 8 1 14) "paragon" in
      parse "import paragon.Policy;" fileName
        `shouldBe`
      Right (CompilationUnit cuSrcSpan Nothing [SingleTypeImport impdSrcSpan (QId qIdSrcSpan policyId AmbigName (Just paragonQId))] [])

    it "parses import declaration for all the types in a package" $
      let fileName = "ImportDecl"
          cuSrcSpan = SrcSpan fileName 1 1 1 17
          impdSrcSpan = SrcSpan fileName 1 1 1 17
          qIdSrcSpan = SrcSpan fileName 1 8 1 14
          paragonId = Id (SrcSpan fileName 1 8 1 14) "paragon" in
      parse "import paragon.*;" fileName
        `shouldBe`
      Right (CompilationUnit cuSrcSpan Nothing [TypeImportOnDemand impdSrcSpan (QId qIdSrcSpan paragonId AmbigName Nothing)] [])

    it "parses static import declaration of a single type" $
      let fileName = "ImportDecl"
          cuSrcSpan = SrcSpan fileName 1 1 1 27
          impdSrcSpan = SrcSpan fileName 1 1 1 27
          qIdSrcSpan = SrcSpan fileName 1 15 1 26
          piId = Id (SrcSpan fileName 1 25 1 26) "PI"
          mathQId = QId (SrcSpan fileName 1 15 1 23) mathId AmbigName (Just langQId)
          mathId = Id (SrcSpan fileName 1 20 1 23) "Math"
          langQId = QId (SrcSpan fileName 1 15 1 18) langId AmbigName Nothing
          langId = Id (SrcSpan fileName 1 15 1 18) "lang" in
      parse "import static lang.Math.PI;" fileName
        `shouldBe`
      Right (CompilationUnit cuSrcSpan Nothing [SingleStaticImport impdSrcSpan (QId qIdSrcSpan piId AmbigName (Just mathQId))] [])

    it "parses static import declaration for all members" $
      let fileName = "ImportDecl"
          cuSrcSpan = SrcSpan fileName 1 1 1 26
          impdSrcSpan = SrcSpan fileName 1 1 1 26
          qIdSrcSpan = SrcSpan fileName 1 15 1 23
          mathId = Id (SrcSpan fileName 1 20 1 23) "Math"
          langQId = QId (SrcSpan fileName 1 15 1 18) langId AmbigName Nothing
          langId = Id (SrcSpan fileName 1 15 1 18) "lang" in
      parse "import static lang.Math.*;" fileName
        `shouldBe`
      Right (CompilationUnit cuSrcSpan Nothing [StaticImportOnDemand impdSrcSpan (QId qIdSrcSpan mathId AmbigName (Just langQId))] [])

    it "parses semicolon type declaration" $
      let fileName = "SemiColonDecl"
          cuSrcSpan = SrcSpan fileName 1 1 1 1 in
      parse ";" fileName
        `shouldBe`
      Right (CompilationUnit cuSrcSpan Nothing [] [])

    it "parses empty class declaration" $
      let fileName = "ClassDecl"
          cuSrcSpan = SrcSpan fileName 1 1 1 10
          ctdSrcSpan = SrcSpan fileName 1 1 1 10
          cd = ClassDecl cdSrcSpan [] cId [] Nothing [] CB
          cdSrcSpan = SrcSpan fileName 1 1 1 10
          cId = Id (SrcSpan fileName 1 7 1 7) "C" in
      parse "class C {}" fileName
        `shouldBe`
      Right (CompilationUnit cuSrcSpan Nothing [] [ClassTypeDecl ctdSrcSpan cd])

    it "parses empty interface declaration" $
      let fileName = "InterfaceDecl"
          cuSrcSpan = SrcSpan fileName 1 1 1 14
          inttdSrcSpan = SrcSpan fileName 1 1 1 14
          intd = InterfaceDecl intdSrcSpan [] intId [] [] IB
          intdSrcSpan = SrcSpan fileName 1 1 1 14
          intId = Id (SrcSpan fileName 1 11 1 11) "I" in
      parse "interface I {}" fileName
        `shouldBe`
      Right (CompilationUnit cuSrcSpan Nothing [] [InterfaceTypeDecl inttdSrcSpan intd])

    -- Failure
    context "given a package declaration with missing semicolon" $
      it "gives an error message" $
        let Left err = parse "package paragon" "PkgDecl"
        in show err `shouldBe` "\"PkgDecl\" (line 1, column 9):\n\
                                \unexpected end of input\n\
                                \expecting . or ;"

    context "given a package declaration with missing package name" $
      it "gives an error message" $
        let Left err = parse "package ;" "PkgDecl"
        in show err `shouldBe` "\"PkgDecl\" (line 1, column 9):\n\
                                \unexpected ;\n\
                                \expecting package name"

    context "given a package declaration with a typo in package keyword" $
      it "gives an error message" $
        let Left err = parse "packag paragon;" "PkgDecl"
        in show err `shouldBe` "\"PkgDecl\" (line 1, column 1):\n\
                                \unexpected packag\n\
                                \expecting package declaration, import declarations, type declarations or end of input"

    context "given an import declaration with missing semicolon" $
      it "gives an error message" $
        let Left err = parse "import paragon" "ImportDecl"
        in show err `shouldBe` "\"ImportDecl\" (line 1, column 8):\n\
                                \unexpected end of input\n\
                                \expecting . or ;"

    context "given an import declaration with missing package name" $
      it "gives an error message" $
        let Left err = parse "import ;" "ImportDecl"
        in show err `shouldBe` "\"ImportDecl\" (line 1, column 8):\n\
                                \unexpected ;\n\
                                \expecting keyword static or package/type name"

    context "given an unfinished import declaration" $
      it "gives an error message" $
        let Left err = parse "import paragon.;" "ImportDecl"
        in show err `shouldBe` "\"ImportDecl\" (line 1, column 16):\n\
                                \unexpected ;\n\
                                \expecting * or identifier"

    context "given a class declaration with missing name" $
      it "gives an error message" $
        let Left err = parse "class {}" "ClassDecl"
        in show err `shouldBe` "\"ClassDecl\" (line 1, column 7):\n\
                                \unexpected {\n\
                                \expecting class name"

    context "given a class declaration with missing opening brace" $
      it "gives an error message" $
        let Left err = parse "class C }" "ClassDecl"
        in show err `shouldBe` "\"ClassDecl\" (line 1, column 9):\n\
                                \unexpected }\n\
                                \expecting {"

    context "given a class declaration with missing closing brace" $
      it "gives an error message" $
        let Left err = parse "class C {" "ClassDecl"
        in show err `shouldBe` "\"ClassDecl\" (line 1, column 9):\n\
                                \unexpected end of input\n\
                                \expecting }"

    context "given an interface declaration with missing name" $
      it "gives an error message" $
        let Left err = parse "interface {}" "InterfaceDecl"
        in show err `shouldBe` "\"InterfaceDecl\" (line 1, column 11):\n\
                                \unexpected {\n\
                                \expecting interface name"

    context "given a interface declaration with missing opening brace" $
      it "gives an error message" $
        let Left err = parse "interface I }" "InterfaceDecl"
        in show err `shouldBe` "\"InterfaceDecl\" (line 1, column 13):\n\
                                \unexpected }\n\
                                \expecting {"

    context "given a interface declaration with missing closing brace" $
      it "gives an error message" $
        let Left err = parse "interface I {" "InterfaceDecl"
        in show err `shouldBe` "\"InterfaceDecl\" (line 1, column 13):\n\
                                \unexpected end of input\n\
                                \expecting }"

