{-# OPTIONS_GHC
    -fno-warn-orphans
 #-}

-- | Parser tests.
module Language.Java.Paragon.ParserSpec (main, spec) where

import System.FilePath ((</>), (<.>))
import Test.Hspec
import Text.ParserCombinators.Parsec.Error
import Control.Monad (liftM2)
import Control.Applicative ((<$>))

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

-- Configuration

testDir :: FilePath
testDir = "test" </> "Language" </> "Java" </> "Paragon" </> "parser"

successDir :: FilePath
successDir = testDir </> "success"

failureDir :: FilePath
failureDir = testDir </> "failure"

-- | Main specification function.
spec :: Spec
spec = do
  describe "parse" $ do
    -- Success
    it "parses an empty program" $
      let fileName = "Empty.para"
          cuSrcSpan = SrcSpan fileName 1 1 1 1
      in successCase fileName (CompilationUnit cuSrcSpan Nothing [] [])

    it "parses package declaration with single identifier" $
      let fileName = "PkgDeclSingle.para"
          srcSpanFun = SrcSpan fileName
          pdSrcSpan = srcSpanFun 1 1 1 16
          paragonSrcSpan = srcSpanFun 1 9 1 15
          paragonId = Id paragonSrcSpan "paragon"
      in successCase fileName (CompilationUnit pdSrcSpan (Just $ PackageDecl pdSrcSpan (Name paragonSrcSpan paragonId PkgName Nothing)) [] [])

    it "parses package declaration with qualified name" $
      let fileName = "PkgDeclQName.para"
          srcSpanFun = SrcSpan fileName
          pdSrcSpan = srcSpanFun 1 1 1 28
          qIdSrcSpan = srcSpanFun 1 9 1 27
          seSrcSpan = srcSpanFun 1 9 1 10
          seId = Id seSrcSpan "se"
          seName = Name seSrcSpan seId PkgName Nothing
          chalmersId = Id (srcSpanFun 1 12 1 19) "chalmers"
          chalmersName = Name (srcSpanFun 1 9 1 19) chalmersId PkgName (Just seName)
          paragonId = Id (srcSpanFun 1 21 1 27) "paragon"
      in successCase fileName (CompilationUnit pdSrcSpan (Just $ PackageDecl pdSrcSpan (Name qIdSrcSpan paragonId PkgName (Just chalmersName))) [] [])

    -- TODO: check correctness of name types
    it "parses single type import declaration" $
      let fileName = "ImportDeclSingle.para"
          srcSpanFun = SrcSpan fileName
          impdSrcSpan = srcSpanFun 1 1 1 22
          qIdSrcSpan = srcSpanFun 1 8 1 21
          policyId = Id (srcSpanFun 1 16 1 21) "Policy"
          paragonSrcSpan = srcSpanFun 1 8 1 14
          paragonId = Id paragonSrcSpan "paragon"
          paragonName = Name paragonSrcSpan paragonId TypeName Nothing
      in successCase fileName (CompilationUnit impdSrcSpan Nothing [SingleTypeImport impdSrcSpan (Name qIdSrcSpan policyId TypeName (Just paragonName))] [])

    it "parses import declaration for all the types in a package" $
      let fileName = "ImportDeclTypeOnDemand.para"
          srcSpanFun = SrcSpan fileName
          impdSrcSpan = srcSpanFun 1 1 1 17
          paragonSrcSpan = srcSpanFun 1 8 1 14
          paragonId = Id paragonSrcSpan "paragon"
      in successCase fileName (CompilationUnit impdSrcSpan Nothing [TypeImportOnDemand impdSrcSpan (Name paragonSrcSpan paragonId PkgOrTypeName Nothing)] [])

    it "parses static import declaration of a single type" $
      let fileName = "ImportDeclSingleStatic.para"
          srcSpanFun = SrcSpan fileName
          impdSrcSpan = srcSpanFun 1 1 1 27
          qIdSrcSpan = srcSpanFun 1 15 1 26
          piId = Id (srcSpanFun 1 25 1 26) "PI"
          mathName = Name (srcSpanFun 1 15 1 23) mathId TypeName (Just langName)
          mathId = Id (srcSpanFun 1 20 1 23) "Math"
          langSrcSpan = srcSpanFun 1 15 1 18
          langName = Name langSrcSpan langId TypeName Nothing
          langId = Id langSrcSpan "lang"
      in successCase fileName (CompilationUnit impdSrcSpan Nothing [SingleStaticImport impdSrcSpan (Name qIdSrcSpan piId AmbigName (Just mathName))] [])

    it "parses static import declaration for all members" $
      let fileName = "ImportDeclStaticOnDemand.para"
          srcSpanFun = SrcSpan fileName
          impdSrcSpan = srcSpanFun 1 1 1 26
          qIdSrcSpan = srcSpanFun 1 15 1 23
          mathId = Id (srcSpanFun 1 20 1 23) "Math"
          langSrcSpan = srcSpanFun 1 15 1 18
          langName = Name langSrcSpan langId TypeName Nothing
          langId = Id langSrcSpan "lang"
      in successCase fileName (CompilationUnit impdSrcSpan Nothing [StaticImportOnDemand impdSrcSpan (Name qIdSrcSpan mathId TypeName (Just langName))] [])

    it "parses semicolon type declaration" $
      let fileName = "SemiColonDecl.para"
          cuSrcSpan = SrcSpan fileName 1 1 1 1
      in successCase fileName (CompilationUnit cuSrcSpan Nothing [] [])

    it "parses empty class declaration" $
      let fileName = "ClassDeclEmpty.para"
          srcSpanFun = SrcSpan fileName
          cdSrcSpan = srcSpanFun 1 1 1 10
          cbSrcSpan = srcSpanFun 1 9 1 10
          cd = ClassDecl cdSrcSpan [] cId [] Nothing [] (ClassBody cbSrcSpan [])
          cId = Id (srcSpanFun 1 7 1 7) "C"
      in successCase fileName (CompilationUnit cdSrcSpan Nothing [] [ClassTypeDecl cdSrcSpan cd])

    it "parses empty interface declaration" $
      let fileName = "InterfaceDeclEmpty.para"
          srcSpanFun = SrcSpan fileName
          intdSrcSpan = srcSpanFun 1 1 1 14
          intd = InterfaceDecl intdSrcSpan [] intId [] [] IB
          intId = Id (srcSpanFun 1 11 1 11) "I"
      in successCase fileName (CompilationUnit intdSrcSpan Nothing [] [InterfaceTypeDecl intdSrcSpan intd])

    it "parses empty class declaration with modifiers" $
      let fileName = "ClassDeclMod.para"
          srcSpanFun = SrcSpan fileName
          cdSrcSpan = srcSpanFun 1 1 1 17
          pblSrcSpan = srcSpanFun 1 1 1 6
          cbSrcSpan = srcSpanFun 1 16 1 17
          cd = ClassDecl cdSrcSpan [Public pblSrcSpan] cId [] Nothing [] (ClassBody cbSrcSpan [])
          cId = Id (srcSpanFun 1 14 1 14) "C"
      in successCase fileName (CompilationUnit cdSrcSpan Nothing [] [ClassTypeDecl cdSrcSpan cd])

    it "parses empty interface declaration with modifiers" $
      let fileName = "InterfaceDeclMod.para"
          srcSpanFun = SrcSpan fileName
          intdSrcSpan = srcSpanFun 1 1 1 21
          pblSrcSpan = srcSpanFun 1 1 1 6
          intd = InterfaceDecl intdSrcSpan [Public pblSrcSpan] intId [] [] IB
          intId = Id (srcSpanFun 1 18 1 18) "I"
      in successCase fileName (CompilationUnit intdSrcSpan Nothing [] [InterfaceTypeDecl intdSrcSpan intd])

    it "parses class declaration with single field declaration" $
      let fileName = "ClassDeclSingleField.para"
          srcSpanFun = SrcSpan fileName
          cdSrcSpan = srcSpanFun 1 1 3 1
          cbSrcSpan = srcSpanFun 1 9 3 1
          mdSrcSpan = srcSpanFun 2 3 2 12
          cbDecl = MemberDecl mdSrcSpan fieldDecl
          tSrcSpan = srcSpanFun 2 3 2 9
          fieldDecl = FieldDecl mdSrcSpan [] (PrimType tSrcSpan (BooleanT tSrcSpan)) [varDecl]
          varId = Id varSrcSpan "x"
          varSrcSpan = srcSpanFun 2 11 2 11
          varDecl = VarDecl varSrcSpan varId
          cd = ClassDecl cdSrcSpan [] cId [] Nothing [] (ClassBody cbSrcSpan [cbDecl])
          cId = Id (srcSpanFun 1 7 1 7) "C"
      in successCase fileName (CompilationUnit cdSrcSpan Nothing [] [ClassTypeDecl cdSrcSpan cd])

    it "parses class declaration with single field declaration with modifiers" $
      let fileName = "ClassDeclSingleFieldMod.para"
          srcSpanFun = SrcSpan fileName
          cdSrcSpan = srcSpanFun 1 1 3 1
          cbSrcSpan = srcSpanFun 1 9 3 1
          mdSrcSpan = srcSpanFun 2 3 2 32
          cbDecl = MemberDecl mdSrcSpan fieldDecl
          tSrcSpan = srcSpanFun 2 23 2 29
          mods = [Public $ srcSpanFun 2 3 2 8, Static $ srcSpanFun 2 10 2 15, Final $ srcSpanFun 2 17 2 21]
          fieldDecl = FieldDecl mdSrcSpan mods (PrimType tSrcSpan (BooleanT tSrcSpan)) [varDecl]
          varId = Id varSrcSpan "B"
          varSrcSpan = srcSpanFun 2 31 2 31
          varDecl = VarDecl varSrcSpan varId
          cd = ClassDecl cdSrcSpan [] cId [] Nothing [] (ClassBody cbSrcSpan [cbDecl])
          cId = Id (srcSpanFun 1 7 1 7) "C"
      in successCase fileName (CompilationUnit cdSrcSpan Nothing [] [ClassTypeDecl cdSrcSpan cd])

    it "parses class declaration with multiple field declarations with modifiers" $
      let fileName = "ClassDeclMultFields.para"
          srcSpanFun = SrcSpan fileName
          cdSrcSpan = srcSpanFun 1 1 4 1
          cbSrcSpan = srcSpanFun 1 9 4 1
          cbDs = [MemberDecl mdSrcSpan1 fieldDecl1, MemberDecl mdSrcSpan2 fieldDecl2]
          mdSrcSpan1 = srcSpanFun 2 3 2 19
          mdSrcSpan2 = srcSpanFun 3 3 3 15
          tSrcSpan1 = srcSpanFun 2 10 2 16
          tSrcSpan2 = srcSpanFun 3 3 3 9
          fieldDecl1 = FieldDecl mdSrcSpan1 [Public $ srcSpanFun 2 3 2 8] (PrimType tSrcSpan1 (BooleanT tSrcSpan1)) [varDecl1]
          fieldDecl2 = FieldDecl mdSrcSpan2 [] (PrimType tSrcSpan2 (BooleanT tSrcSpan2)) [varDecl2, varDecl3]
          varId1 = Id varSrcSpan1 "x"
          varSrcSpan1 = srcSpanFun 2 18 2 18
          varId2 = Id varSrcSpan2 "y"
          varSrcSpan2 = srcSpanFun 3 11 3 11
          varId3 = Id varSrcSpan3 "z"
          varSrcSpan3 = srcSpanFun 3 14 3 14
          varDecl1 = VarDecl varSrcSpan1 varId1
          varDecl2 = VarDecl varSrcSpan2 varId2
          varDecl3 = VarDecl varSrcSpan3 varId3
          cd = ClassDecl cdSrcSpan [] cId [] Nothing [] (ClassBody cbSrcSpan cbDs)
          cId = Id (srcSpanFun 1 7 1 7) "C"
      in successCase fileName (CompilationUnit cdSrcSpan Nothing [] [ClassTypeDecl cdSrcSpan cd])

    it "parses class declaration with void method with semicolon body" $
      let fileName = "ClassDeclVoidMethodSemiColon.para"
          srcSpanFun = SrcSpan fileName
          cdSrcSpan = srcSpanFun 1 1 3 1
          cbSrcSpan = srcSpanFun 1 9 3 1
          mdSrcSpan = srcSpanFun 2 3 2 11
          cbDecl = MemberDecl mdSrcSpan methodDecl
          mId = Id (srcSpanFun 2 8 2 8) "f"
          body = MethodBody (srcSpanFun 2 11 2 11) Nothing
          methodDecl = MethodDecl mdSrcSpan [] [] (VoidType (srcSpanFun 2 3 2 6)) mId [] body
          cd = ClassDecl cdSrcSpan [] cId [] Nothing [] (ClassBody cbSrcSpan [cbDecl])
          cId = Id (srcSpanFun 1 7 1 7) "C"
      in successCase fileName (CompilationUnit cdSrcSpan Nothing [] [ClassTypeDecl cdSrcSpan cd])

    it "parses class declaration with int method with semicolon body" $
      let fileName = "ClassDeclIntMethodSemiColon.para"
          srcSpanFun = SrcSpan fileName
          cdSrcSpan = srcSpanFun 1 1 3 1
          cbSrcSpan = srcSpanFun 1 9 3 1
          mdSrcSpan = srcSpanFun 2 3 2 10
          cbDecl = MemberDecl mdSrcSpan methodDecl
          mId = Id (srcSpanFun 2 7 2 7) "f"
          body = MethodBody (srcSpanFun 2 10 2 10) Nothing
          intTSrcSpan = srcSpanFun 2 3 2 5
          intT = PrimType intTSrcSpan (IntT intTSrcSpan)
          methodDecl = MethodDecl mdSrcSpan [] [] (Type intTSrcSpan intT) mId [] body
          cd = ClassDecl cdSrcSpan [] cId [] Nothing [] (ClassBody cbSrcSpan [cbDecl])
          cId = Id (srcSpanFun 1 7 1 7) "C"
      in successCase fileName (CompilationUnit cdSrcSpan Nothing [] [ClassTypeDecl cdSrcSpan cd])

    it "parses class declaration with void method with empty body" $
      let fileName = "ClassDeclVoidMethodEmptyBody.para"
          srcSpanFun = SrcSpan fileName
          cdSrcSpan = srcSpanFun 1 1 3 1
          cbSrcSpan = srcSpanFun 1 9 3 1
          mdSrcSpan = srcSpanFun 2 3 2 13
          cbDecl = MemberDecl mdSrcSpan methodDecl
          mId = Id (srcSpanFun 2 8 2 8) "f"
          bodySrcSpan = srcSpanFun 2 12 2 13
          body = MethodBody bodySrcSpan (Just (Block bodySrcSpan []))
          methodDecl = MethodDecl mdSrcSpan [] [] (VoidType (srcSpanFun 2 3 2 6)) mId [] body
          cd = ClassDecl cdSrcSpan [] cId [] Nothing [] (ClassBody cbSrcSpan [cbDecl])
          cId = Id (srcSpanFun 1 7 1 7) "C"
      in successCase fileName (CompilationUnit cdSrcSpan Nothing [] [ClassTypeDecl cdSrcSpan cd])

    it "parses class declaration with int method with empty body with modifiers" $
      let fileName = "ClassDeclIntMethodModEmptyBody.para"
          srcSpanFun = SrcSpan fileName
          cdSrcSpan = srcSpanFun 1 1 3 1
          cbSrcSpan = srcSpanFun 1 9 3 1
          mdSrcSpan = srcSpanFun 2 3 2 26
          cbDecl = MemberDecl mdSrcSpan methodDecl
          mId = Id (srcSpanFun 2 21 2 21) "f"
          bodySrcSpan = srcSpanFun 2 25 2 26
          body = MethodBody bodySrcSpan (Just (Block bodySrcSpan []))
          intTSrcSpan = srcSpanFun 2 17 2 19
          intT = PrimType intTSrcSpan (IntT intTSrcSpan)
          mods = [Public $ srcSpanFun 2 3 2 8, Static $ srcSpanFun 2 10 2 15]
          methodDecl = MethodDecl mdSrcSpan mods [] (Type intTSrcSpan intT) mId [] body
          cd = ClassDecl cdSrcSpan [] cId [] Nothing [] (ClassBody cbSrcSpan [cbDecl])
          cId = Id (srcSpanFun 1 7 1 7) "C"
      in successCase fileName (CompilationUnit cdSrcSpan Nothing [] [ClassTypeDecl cdSrcSpan cd])

    it "parses class declaration with void method with empty statement" $
      let fileName = "ClassDeclVoidMethodSemiColonStmt.para"
          srcSpanFun = SrcSpan fileName
          cdSrcSpan = srcSpanFun 1 1 5 1
          cbSrcSpan = srcSpanFun 1 9 5 1
          mdSrcSpan = srcSpanFun 2 3 4 3
          cbDecl = MemberDecl mdSrcSpan methodDecl
          mId = Id (srcSpanFun 2 8 2 8) "f"
          bodySrcSpan = srcSpanFun 2 12 4 3
          stmtSrcSpan = srcSpanFun 3 5 3 5
          body = MethodBody bodySrcSpan (Just (Block bodySrcSpan [BlockStmt stmtSrcSpan (Empty stmtSrcSpan)]))
          methodDecl = MethodDecl mdSrcSpan [] [] (VoidType (srcSpanFun 2 3 2 6)) mId [] body
          cd = ClassDecl cdSrcSpan [] cId [] Nothing [] (ClassBody cbSrcSpan [cbDecl])
          cId = Id (srcSpanFun 1 7 1 7) "C"
      in successCase fileName (CompilationUnit cdSrcSpan Nothing [] [ClassTypeDecl cdSrcSpan cd])

    it "parses class declaration with void method with single local variable declaration" $
      let fileName = "ClassDeclVoidMethodSingleLocalVarDecl.para"
          srcSpanFun = SrcSpan fileName
          cdSrcSpan = srcSpanFun 1 1 5 1
          cbSrcSpan = srcSpanFun 1 9 5 1
          mdSrcSpan = srcSpanFun 2 3 4 3
          cbDecl = MemberDecl mdSrcSpan methodDecl
          mId = Id (srcSpanFun 2 8 2 8) "f"
          bodySrcSpan = srcSpanFun 2 12 4 3
          stmtSrcSpan = srcSpanFun 3 5 3 12
          floatTSrcSpan = srcSpanFun 3 5 3 9
          floatT = PrimType floatTSrcSpan (FloatT floatTSrcSpan)
          varId = Id varSrcSpan "x"
          varSrcSpan = srcSpanFun 3 11 3 11
          varDecl = VarDecl varSrcSpan varId
          body = MethodBody bodySrcSpan (Just (Block bodySrcSpan [LocalVars stmtSrcSpan [] floatT [varDecl]]))
          methodDecl = MethodDecl mdSrcSpan [] [] (VoidType (srcSpanFun 2 3 2 6)) mId [] body
          cd = ClassDecl cdSrcSpan [] cId [] Nothing [] (ClassBody cbSrcSpan [cbDecl])
          cId = Id (srcSpanFun 1 7 1 7) "C"
      in successCase fileName (CompilationUnit cdSrcSpan Nothing [] [ClassTypeDecl cdSrcSpan cd])

    it "parses class declaration with void method with single local variable declaration with modifier" $
      let fileName = "ClassDeclVoidMethodSingleLocalVarDeclMod.para"
          srcSpanFun = SrcSpan fileName
          cdSrcSpan = srcSpanFun 1 1 5 1
          cbSrcSpan = srcSpanFun 1 9 5 1
          mdSrcSpan = srcSpanFun 2 3 4 3
          cbDecl = MemberDecl mdSrcSpan methodDecl
          mId = Id (srcSpanFun 2 8 2 8) "f"
          bodySrcSpan = srcSpanFun 2 12 4 3
          stmtSrcSpan = srcSpanFun 3 5 3 16
          intTSrcSpan = srcSpanFun 3 11 3 13
          intT = PrimType intTSrcSpan (IntT intTSrcSpan)
          varId = Id varSrcSpan "x"
          varSrcSpan = srcSpanFun 3 15 3 15
          varDecl = VarDecl varSrcSpan varId
          mods = [Final $ srcSpanFun 3 5 3 9]
          body = MethodBody bodySrcSpan (Just (Block bodySrcSpan [LocalVars stmtSrcSpan mods intT [varDecl]]))
          methodDecl = MethodDecl mdSrcSpan [] [] (VoidType (srcSpanFun 2 3 2 6)) mId [] body
          cd = ClassDecl cdSrcSpan [] cId [] Nothing [] (ClassBody cbSrcSpan [cbDecl])
          cId = Id (srcSpanFun 1 7 1 7) "C"
      in successCase fileName (CompilationUnit cdSrcSpan Nothing [] [ClassTypeDecl cdSrcSpan cd])

    it "parses class declaration with void method with multiple local variable declarations" $
      let fileName = "ClassDeclVoidMethodMultLocalVarDecls.para"
          srcSpanFun = SrcSpan fileName
          cdSrcSpan = srcSpanFun 1 1 6 1
          cbSrcSpan = srcSpanFun 1 9 6 1
          mdSrcSpan = srcSpanFun 2 3 5 3
          cbDecl = MemberDecl mdSrcSpan methodDecl
          mId = Id (srcSpanFun 2 8 2 8) "f"
          bodySrcSpan = srcSpanFun 2 12 5 3
          stmtSrcSpan1 = srcSpanFun 3 5 3 11
          stmtSrcSpan2 = srcSpanFun 4 5 4 16
          byteTSrcSpan = srcSpanFun 3 5 3 8
          byteT = PrimType byteTSrcSpan (ByteT byteTSrcSpan)
          doubleTSrcSpan = srcSpanFun 4 5 4 10
          doubleT = PrimType doubleTSrcSpan (DoubleT doubleTSrcSpan)
          varId1 = Id varSrcSpan1 "x"
          varSrcSpan1 = srcSpanFun 3 10 3 10
          varDecl1 = VarDecl varSrcSpan1 varId1
          varId2 = Id varSrcSpan2 "y"
          varSrcSpan2 = srcSpanFun 4 12 4 12
          varDecl2 = VarDecl varSrcSpan2 varId2
          varId3 = Id varSrcSpan3 "z"
          varSrcSpan3 = srcSpanFun 4 15 4 15
          varDecl3 = VarDecl varSrcSpan3 varId3
          body = MethodBody bodySrcSpan (Just (Block bodySrcSpan [ LocalVars stmtSrcSpan1 [] byteT [varDecl1]
                                                                 , LocalVars stmtSrcSpan2 [] doubleT [varDecl2, varDecl3]]))
          methodDecl = MethodDecl mdSrcSpan [] [] (VoidType (srcSpanFun 2 3 2 6)) mId [] body
          cd = ClassDecl cdSrcSpan [] cId [] Nothing [] (ClassBody cbSrcSpan [cbDecl])
          cId = Id (srcSpanFun 1 7 1 7) "C"
      in successCase fileName (CompilationUnit cdSrcSpan Nothing [] [ClassTypeDecl cdSrcSpan cd])

    it "parses class declaration with void method with single assignment expression statement with literal" $
      let fileName = "ClassDeclVoidMethodSingleAssignLit.para"
          srcSpanFun = SrcSpan fileName
          cdSrcSpan = srcSpanFun 1 1 5 1
          cbSrcSpan = srcSpanFun 1 9 5 1
          mdSrcSpan = srcSpanFun 2 3 4 3
          cbDecl = MemberDecl mdSrcSpan methodDecl
          mId = Id (srcSpanFun 2 8 2 8) "f"
          bodySrcSpan = srcSpanFun 2 12 4 3
          stmtSrcSpan = srcSpanFun 3 5 3 10
          assignSrcSpan = srcSpanFun 3 5 3 9
          varId = Id varSrcSpan "x"
          varSrcSpan = srcSpanFun 3 5 3 5
          litSrcSpan = srcSpanFun 3 9 3 9
          assign = Assign assignSrcSpan (NameLhs varSrcSpan (Name varSrcSpan varId ExpName Nothing)) (EqualA $ srcSpanFun 3 7 3 7) (Lit litSrcSpan (Int litSrcSpan 1))
          body = MethodBody bodySrcSpan (Just (Block bodySrcSpan [BlockStmt stmtSrcSpan (ExpStmt stmtSrcSpan assign)]))
          methodDecl = MethodDecl mdSrcSpan [] [] (VoidType (srcSpanFun 2 3 2 6)) mId [] body
          cd = ClassDecl cdSrcSpan [] cId [] Nothing [] (ClassBody cbSrcSpan [cbDecl])
          cId = Id (srcSpanFun 1 7 1 7) "C"
      in successCase fileName (CompilationUnit cdSrcSpan Nothing [] [ClassTypeDecl cdSrcSpan cd])

    -- Failure
    describe "gives an error message when" $ do

      it "given a package declaration with missing semicolon" $
        failureCase "PkgDeclMissSemiColon"

      it "given a package declaration with missing package name" $
        failureCase "PkgDeclMissPkgName"

      it "given a package declaration with a typo in package keyword" $
        failureCase "PkgDeclKeywordTypo"

      it "given an import declaration with missing semicolon" $
        failureCase "ImportDeclMissSemiColon"

      it "given an import declaration with missing package name" $
        failureCase "ImportDeclMissPkgName"

      it "given an unfinished import declaration" $
        failureCase "ImportDeclUnfinished"

      it "given a class declaration with missing name" $
        failureCase "ClassDeclMissName"

      it "given a class declaration with missing opening brace" $
        failureCase "ClassDeclMissOpenBrace"

      it "given a class declaration with missing closing brace" $
        failureCase "ClassDeclMissCloseBrace"

      it "given a class declaration with missing body" $
        failureCase "ClassDeclMissBody"

      it "given an interface declaration with missing name" $
        failureCase "InterfaceDeclMissName"

      it "given an interface declaration with missing opening brace" $
        failureCase "InterfaceDeclMissOpenBrace"

      it "given an interface declaration with missing closing brace" $
        failureCase "InterfaceDeclMissCloseBrace"

      it "given a class declaration with misspelled modifier" $
        failureCase "ClassDeclModTypo"

      it "given an interface declaration with misspelled modifier" $
        failureCase "InterfaceDeclModTypo"

      -- Error message should be improved
      it "given a class declaration with single field declaration with missing name" $
        failureCase "ClassDeclSingleFieldMissName"

      it "given a class declaration with single field declaration with missing type" $
        failureCase "ClassDeclSingleFieldMissType"

      -- Error message should be improved
      it "given a class declaration with single field declaration with missing semicolon" $
        failureCase "ClassDeclSingleFieldMissSemiColon"

      it "given a class declaration with single field declaration with modifier and missing type" $
        failureCase "ClassDeclSingleFieldModMissType"

      -- Error message should be improved
      it "given a class declaration with single field and comma" $
        failureCase "ClassDeclSingleFieldComma"

      it "given a class declaration with void method with missing name with semicolon body" $
       failureCase "ClassDeclVoidMethodSemiColonMissName"

      it "given a class declaration with int method with missing name with semicolon body" $
       failureCase "ClassDeclIntMethodSemiColonMissName"

      it "given a class declaration with method with missing return type with semicolon body" $
       failureCase "ClassDeclMethodSemiColonMissRetType"

      it "given a class declaration with int method with missing opening paren with semicolon body" $
       failureCase "ClassDeclIntMethodSemiColonMissOpenParen"

      it "given a class declaration with int method with missing closing paren with semicolon body" $
       failureCase "ClassDeclIntMethodSemiColonMissCloseParen"

      it "given a class declaration with int method with missing body" $
       failureCase "ClassDeclIntMethodMissBody"

      it "given a class declaration with void method with missing opening curly" $
       failureCase "ClassDeclIntMethodMissOpenCurly"

      it "given a class declaration with int method with missing closing curly" $
       failureCase "ClassDeclIntMethodMissCloseCurly"

      it "given a class declaration with void method with local variable declaration with missing semicolon" $
        failureCase "ClassDeclVoidMethodSingleLocalVarDeclMissSemiColon"

      it "given a class declaration with void method with local variable declaration with missing name" $
        failureCase "ClassDeclVoidMethodSingleLocalVarDeclMissName"

      it "given a class declaration with void method with local variable declaration and extra comma" $
        failureCase "ClassDeclVoidMethodSingleLocalVarDeclComma"

      it "given a class declaration with void method with void local variable declaration" $
        failureCase "ClassDeclVoidMethodSingleLocalVarDeclVoid"

      it "given a class declaration with void method with local variable declaration with misspelled modifier" $
        failureCase "ClassDeclVoidMethodSingleLocalVarDeclModTypo"

      it "given a class declaration with void method with assignment with missing left-hand side" $
        failureCase "ClassDeclVoidMethodAssignMissLhs"

      it "given a class declaration with void method with assignment with missing right-hand side" $
        failureCase "ClassDeclVoidMethodAssignMissRhs"

      it "given a class declaration with void method with assignment with missing semicolon" $
        failureCase "ClassDeclVoidMethodAssignMissSemiColon"

-- Infrastructure

successCase :: String -> AST SrcSpan -> IO ()
successCase fileName result = do
  input <- successRead fileName
  parse input fileName `shouldBe` Right result

successRead :: String -> IO String
successRead fileName = readFile (successDir </> fileName)

failureCase :: String -> IO ()
failureCase baseName = do
  (input, errMsg) <- failureRead baseName
  let Left err = parse input (baseName <.> "para")
  show err `shouldBe` errMsg

failureRead :: String -> IO (String, String)
failureRead baseName = liftM2 (,) (readFile (failureDir </> baseName <.> "para"))
                                  (dropNewLine <$> readFile (failureDir </> baseName <.> "err"))
  where dropNewLine ""  = ""
        dropNewLine str = let l = last str
                          in if l == '\n' || l == '\r'
                               then dropNewLine (init str)
                               else str

