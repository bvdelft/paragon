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

import Language.Java.Paragon.Annotation
import Language.Java.Paragon.Syntax
import Language.Java.Paragon.Parser
import Language.Java.Paragon.SrcPos
import Language.Java.Paragon.ASTHelpers

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
testDir = "test" </> "parsertests"

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
      let baseName = "Empty"
          fileName = mkFileName baseName
          cuSrcSpan = makeSrcSpanAnn fileName 1 1 1 1
      in successCase baseName (simpleCompilationUnit cuSrcSpan [])

    it "parses package declaration with single identifier" $
      let baseName = "PkgDeclSingle"
          fileName = mkFileName baseName
          srcSpanFun = makeSrcSpanAnn fileName
          ast = CompilationUnit (srcSpanFun 1 1 1 16)
                  (Just $ PackageDecl (srcSpanFun 1 1 1 16)
                     (simpleName (srcSpanFun 1 9 1 15) "paragon" PkgName))
                  []
                  []
      in successCase baseName ast

    it "parses package declaration with qualified name" $
      let baseName = "PkgDeclQName"
          fileName = mkFileName baseName
          srcSpanFun = makeSrcSpanAnn fileName
          pdSrcSpan = srcSpanFun 1 1 1 28
          ast = CompilationUnit pdSrcSpan
                  (Just $ PackageDecl pdSrcSpan
                     (Name (srcSpanFun 1 9 1 27)
                        (Id (srcSpanFun 1 21 1 27) "paragon")
                        PkgName
                        (Just $ Name (srcSpanFun 1 9 1 19)
                                  (Id (srcSpanFun 1 12 1 19) "chalmers")
                                  PkgName
                                  (Just $ simpleName (srcSpanFun 1 9 1 10)
                                            "se"
                                            PkgName))))
                []
                []
      in successCase baseName ast

    it "parses single type import declaration" $
      let baseName = "ImportDeclSingle"
          fileName = mkFileName baseName
          srcSpanFun = makeSrcSpanAnn fileName
          impdSrcSpan = srcSpanFun 1 1 1 22
          ast = CompilationUnit impdSrcSpan
                  Nothing
                  [SingleTypeImport impdSrcSpan
                     (Name (srcSpanFun 1 8 1 21)
                        (Id (srcSpanFun 1 16 1 21) "Policy")
                        TypeName
                        (Just $ simpleName (srcSpanFun 1 8 1 14)
                                  "paragon"
                                  PkgOrTypeName))]
                  []
      in successCase baseName ast

    it "parses import declaration for all the types in a package" $
      let baseName = "ImportDeclTypeOnDemand"
          fileName = mkFileName baseName
          srcSpanFun = makeSrcSpanAnn fileName
          impdSrcSpan = srcSpanFun 1 1 1 17
          ast = CompilationUnit impdSrcSpan
                  Nothing
                  [TypeImportOnDemand impdSrcSpan
                     (simpleName (srcSpanFun 1 8 1 14)
                        "paragon"
                        PkgOrTypeName)]
                  []
      in successCase baseName ast

    it "parses static import declaration of a single type" $
      let baseName = "ImportDeclSingleStatic"
          fileName = mkFileName baseName
          srcSpanFun = makeSrcSpanAnn fileName
          impdSrcSpan = srcSpanFun 1 1 1 27
          ast = CompilationUnit impdSrcSpan
                  Nothing
                  [SingleStaticImport impdSrcSpan
                     (Name (srcSpanFun 1 15 1 26)
                        (Id (srcSpanFun 1 25 1 26) "PI")
                        AmbigName
                        (Just $ Name (srcSpanFun 1 15 1 23)
                                  (Id (srcSpanFun 1 20 1 23) "Math")
                                  TypeName
                                  (Just $ simpleName (srcSpanFun 1 15 1 18)
                                            "lang"
                                            TypeName)))]
                  []
      in successCase baseName ast

    it "parses static import declaration for all members" $
      let baseName = "ImportDeclStaticOnDemand"
          fileName = mkFileName baseName
          srcSpanFun = makeSrcSpanAnn fileName
          impdSrcSpan = srcSpanFun 1 1 1 26
          ast = CompilationUnit impdSrcSpan
                  Nothing
                  [StaticImportOnDemand impdSrcSpan
                     (Name (srcSpanFun 1 15 1 23)
                        (Id (srcSpanFun 1 20 1 23) "Math")
                        TypeName
                        (Just $ simpleName (srcSpanFun 1 15 1 18)
                                  "lang"
                                  PkgOrTypeName))]
                  []
      in successCase baseName ast

    it "parses semicolon type declaration" $
      let baseName = "SemiColonDecl"
          fileName = mkFileName baseName
          cuSrcSpan = makeSrcSpanAnn fileName 1 1 1 1
      in successCase baseName (simpleCompilationUnit cuSrcSpan [])

    it "parses empty class declaration" $
      let baseName = "ClassDeclEmpty"
          fileName = mkFileName baseName
          srcSpanFun = makeSrcSpanAnn fileName
          ast = simpleClassDeclCompUnit (srcSpanFun 1 1 1 10)
                  (Id (srcSpanFun 1 7 1 7) "C")
                  (ClassBody (srcSpanFun 1 9 1 10) [])
      in successCase baseName ast

    it "parses empty interface declaration" $
      let baseName = "InterfaceDeclEmpty"
          fileName = mkFileName baseName
          srcSpanFun = makeSrcSpanAnn fileName
          intdSrcSpan = srcSpanFun 1 1 1 14
          ast = CompilationUnit intdSrcSpan
                  Nothing
                  []
                  [InterfaceTypeDecl $
                     InterfaceDecl intdSrcSpan
                       []
                       (Id (srcSpanFun 1 11 1 11) "I")
                       [] [] IB]
      in successCase baseName ast

    it "parses empty class declaration with modifiers" $
      let baseName = "ClassDeclMod"
          fileName = mkFileName baseName
          srcSpanFun = makeSrcSpanAnn fileName
          cdSrcSpan = srcSpanFun 1 1 1 17
          ast = CompilationUnit cdSrcSpan
                  Nothing
                  []
                  [ClassTypeDecl $
                     ClassDecl cdSrcSpan
                       [Public $ srcSpanFun 1 1 1 6]
                       (Id (srcSpanFun 1 14 1 14) "C")
                       []
                       Nothing
                       []
                       (ClassBody (srcSpanFun 1 16 1 17) [])]
      in successCase baseName ast

    it "parses empty interface declaration with modifiers" $
      let baseName = "InterfaceDeclMod"
          fileName = mkFileName baseName
          srcSpanFun = makeSrcSpanAnn fileName
          intdSrcSpan = srcSpanFun 1 1 1 21
          ast = CompilationUnit intdSrcSpan
                  Nothing
                  []
                  [InterfaceTypeDecl $
                     InterfaceDecl intdSrcSpan
                       [Public $ srcSpanFun 1 1 1 6]
                       (Id (srcSpanFun 1 18 1 18) "I")
                       [] [] IB]
      in successCase baseName ast

    it "parses class declaration with single field declaration" $
      let baseName = "ClassDeclSingleField"
          fileName = mkFileName baseName
          srcSpanFun = makeSrcSpanAnn fileName
          ast = simpleClassDeclCompUnit (srcSpanFun 1 1 3 1)
                  (Id (srcSpanFun 1 7 1 7) "C")
                  (ClassBody (srcSpanFun 1 9 3 1)
                     [simpleFieldDecl (srcSpanFun 2 3 2 12)
                        (PrimType $ BooleanT $ srcSpanFun 2 3 2 9)
                        [simpleVarDecl (srcSpanFun 2 11 2 11) "x"]])
      in successCase baseName ast

    it "parses class declaration with single field declaration with modifiers" $
      let baseName = "ClassDeclSingleFieldMod"
          fileName = mkFileName baseName
          srcSpanFun = makeSrcSpanAnn fileName
          ast = simpleClassDeclCompUnit (srcSpanFun 1 1 3 1)
                  (Id (srcSpanFun 1 7 1 7) "C")
                  (ClassBody (srcSpanFun 1 9 3 1)
                     [MemberDecl $
                        FieldDecl (srcSpanFun 2 3 2 32)
                          [ Public $ srcSpanFun 2 3 2 8
                          , Static $ srcSpanFun 2 10 2 15
                          , Final $ srcSpanFun 2 17 2 21]
                          (PrimType $ BooleanT $ srcSpanFun 2 23 2 29)
                          [simpleVarDecl (srcSpanFun 2 31 2 31) "B"]])
      in successCase baseName ast

    it "parses class declaration with multiple field declarations with modifiers" $
      let baseName = "ClassDeclMultFields"
          fileName = mkFileName baseName
          srcSpanFun = makeSrcSpanAnn fileName
          ast = simpleClassDeclCompUnit (srcSpanFun 1 1 4 1)
                  (Id (srcSpanFun 1 7 1 7) "C")
                  (ClassBody (srcSpanFun 1 9 4 1)
                     [ MemberDecl $
                         FieldDecl (srcSpanFun 2 3 2 19)
                           [Public $ srcSpanFun 2 3 2 8]
                           (PrimType $ BooleanT $ srcSpanFun 2 10 2 16)
                           [simpleVarDecl (srcSpanFun 2 18 2 18) "x"]
                     , simpleFieldDecl (srcSpanFun 3 3 3 15)
                         (PrimType $ BooleanT $ srcSpanFun 3 3 3 9)
                         [ simpleVarDecl (srcSpanFun 3 11 3 11) "y"
                         , simpleVarDecl (srcSpanFun 3 14 3 14) "z"]])
      in successCase baseName ast

    it "parses class declaration with void method with semicolon body" $
      let baseName = "ClassDeclVoidMethodSemiColon"
          fileName = mkFileName baseName
          srcSpanFun = makeSrcSpanAnn fileName
          ast = simpleClassDeclCompUnit (srcSpanFun 1 1 3 1)
                  (Id (srcSpanFun 1 7 1 7) "C")
                  (ClassBody (srcSpanFun 1 9 3 1)
                     [simpleMethodDecl (srcSpanFun 2 3 2 11)
                        (VoidType $ srcSpanFun 2 3 2 6)
                        (Id (srcSpanFun 2 8 2 8) "f")
                        []
                        (MethodBody (srcSpanFun 2 11 2 11) Nothing)])
      in successCase baseName ast

    it "parses class declaration with int method with semicolon body" $
      let baseName = "ClassDeclIntMethodSemiColon"
          fileName = mkFileName baseName
          srcSpanFun = makeSrcSpanAnn fileName
          ast = simpleClassDeclCompUnit (srcSpanFun 1 1 3 1)
                  (Id (srcSpanFun 1 7 1 7) "C")
                  (ClassBody (srcSpanFun 1 9 3 1)
                     [simpleMethodDecl (srcSpanFun 2 3 2 10)
                        (primRetType $ IntT $ srcSpanFun 2 3 2 5)
                        (Id (srcSpanFun 2 7 2 7) "f")
                        []
                        (MethodBody (srcSpanFun 2 10 2 10) Nothing)])
      in successCase baseName ast

    it "parses class declaration with void method with empty body" $
      let baseName = "ClassDeclVoidMethodEmptyBody"
          fileName = mkFileName baseName
          srcSpanFun = makeSrcSpanAnn fileName
          ast = simpleClassDeclCompUnit (srcSpanFun 1 1 3 1)
                  (Id (srcSpanFun 1 7 1 7) "C")
                  (ClassBody (srcSpanFun 1 9 3 1)
                     [simpleMethodDecl (srcSpanFun 2 3 2 13)
                        (VoidType $ srcSpanFun 2 3 2 6)
                        (Id (srcSpanFun 2 8 2 8) "f")
                        []
                        (simpleMethodBody (srcSpanFun 2 12 2 13) [])])
      in successCase baseName ast

    it "parses class declaration with int method with empty body with modifiers" $
      let baseName = "ClassDeclIntMethodModEmptyBody"
          fileName = mkFileName baseName
          srcSpanFun = makeSrcSpanAnn fileName
          ast = simpleClassDeclCompUnit (srcSpanFun 1 1 3 1)
                  (Id (srcSpanFun 1 7 1 7) "C")
                  (ClassBody (srcSpanFun 1 9 3 1)
                     [MemberDecl $
                        MethodDecl (srcSpanFun 2 3 2 26)
                          [Public $ srcSpanFun 2 3 2 8, Static $ srcSpanFun 2 10 2 15]
                          []
                          (primRetType $ IntT $ srcSpanFun 2 17 2 19)
                          (Id (srcSpanFun 2 21 2 21) "f")
                          []
                          (simpleMethodBody (srcSpanFun 2 25 2 26) [])])
      in successCase baseName ast

    it "parses class declaration with void method with empty statement" $
      let baseName = "ClassDeclVoidMethodSemiColonStmt"
          fileName = mkFileName baseName
          srcSpanFun = makeSrcSpanAnn fileName
          ast = simpleClassDeclCompUnit (srcSpanFun 1 1 5 1)
                  (Id (srcSpanFun 1 7 1 7) "C")
                  (ClassBody (srcSpanFun 1 9 5 1)
                     [simpleMethodDecl (srcSpanFun 2 3 4 3)
                        (VoidType $ srcSpanFun 2 3 2 6)
                        (Id (srcSpanFun 2 8 2 8) "f")
                        []
                        (simpleMethodBody (srcSpanFun 2 12 4 3)
                           [BlockStmt (Empty $ srcSpanFun 3 5 3 5)])])
      in successCase baseName ast

    it "parses class declaration with void method with single local variable declaration" $
      let baseName = "ClassDeclVoidMethodSingleLocalVarDecl"
          fileName = mkFileName baseName
          srcSpanFun = makeSrcSpanAnn fileName
          ast = simpleClassDeclCompUnit (srcSpanFun 1 1 5 1)
                  (Id (srcSpanFun 1 7 1 7) "C")
                  (ClassBody (srcSpanFun 1 9 5 1)
                     [simpleMethodDecl (srcSpanFun 2 3 4 3)
                        (VoidType $ srcSpanFun 2 3 2 6)
                        (Id (srcSpanFun 2 8 2 8) "f")
                        []
                        (simpleMethodBody (srcSpanFun 2 12 4 3)
                           [LocalVars (srcSpanFun 3 5 3 12)
                              []
                              (PrimType $ FloatT $ srcSpanFun 3 5 3 9)
                              [simpleVarDecl (srcSpanFun 3 11 3 11) "x"]])])
      in successCase baseName ast

    it "parses class declaration with void method with single local variable declaration with modifier" $
      let baseName = "ClassDeclVoidMethodSingleLocalVarDeclMod"
          fileName = mkFileName baseName
          srcSpanFun = makeSrcSpanAnn fileName
          ast = simpleClassDeclCompUnit (srcSpanFun 1 1 5 1)
                  (Id (srcSpanFun 1 7 1 7) "C")
                  (ClassBody (srcSpanFun 1 9 5 1)
                     [simpleMethodDecl (srcSpanFun 2 3 4 3)
                        (VoidType (srcSpanFun 2 3 2 6))
                        (Id (srcSpanFun 2 8 2 8) "f")
                        []
                        (simpleMethodBody (srcSpanFun 2 12 4 3)
                           [LocalVars (srcSpanFun 3 5 3 16)
                              [Final $ srcSpanFun 3 5 3 9]
                              (PrimType $ IntT $ srcSpanFun 3 11 3 13)
                              [simpleVarDecl (srcSpanFun 3 15 3 15) "x"]])])
      in successCase baseName ast

    it "parses class declaration with void method with multiple local variable declarations" $
      let baseName = "ClassDeclVoidMethodMultLocalVarDecls"
          fileName = mkFileName baseName
          srcSpanFun = makeSrcSpanAnn fileName
          ast = simpleClassDeclCompUnit (srcSpanFun 1 1 6 1)
                  (Id (srcSpanFun 1 7 1 7) "C")
                  (ClassBody (srcSpanFun 1 9 6 1)
                     [simpleMethodDecl (srcSpanFun 2 3 5 3)
                        (VoidType $ srcSpanFun 2 3 2 6)
                        (Id (srcSpanFun 2 8 2 8) "f")
                        []
                        (simpleMethodBody (srcSpanFun 2 12 5 3)
                           [ LocalVars (srcSpanFun 3 5 3 11)
                               []
                               (PrimType $ ByteT $ srcSpanFun 3 5 3 8)
                               [simpleVarDecl (srcSpanFun 3 10 3 10) "x"]
                           , LocalVars (srcSpanFun 4 5 4 16)
                               []
                               (PrimType $ DoubleT $ srcSpanFun 4 5 4 10)
                               [ simpleVarDecl (srcSpanFun 4 12 4 12) "y"
                               , simpleVarDecl (srcSpanFun 4 15 4 15) "z"]])])
      in successCase baseName ast

    it "parses class declaration with void method with single assignment expression statement with literal" $
      let baseName = "ClassDeclVoidMethodSingleAssignLit"
          fileName = mkFileName baseName
          srcSpanFun = makeSrcSpanAnn fileName
          assignSrcSpan = srcSpanFun 3 5 3 9
          varSrcSpan = srcSpanFun 3 5 3 5
          litSrcSpan = srcSpanFun 3 9 3 9
          assign = Assign assignSrcSpan (NameLhs $ simpleName varSrcSpan "x" ExpName) (EqualA $ srcSpanFun 3 7 3 7) (Lit $ Int litSrcSpan 1)
          ast = simpleClassDeclCompUnit (srcSpanFun 1 1 5 1)
                  (Id (srcSpanFun 1 7 1 7) "C")
                  (ClassBody (srcSpanFun 1 9 5 1)
                     [simpleMethodDecl (srcSpanFun 2 3 4 3)
                        (VoidType $ srcSpanFun 2 3 2 6)
                        (Id (srcSpanFun 2 8 2 8) "f")
                        []
                        (simpleMethodBody (srcSpanFun 2 12 4 3)
                           [BlockStmt $ ExpStmt (srcSpanFun 3 5 3 10) assign])])
      in successCase baseName ast

    it "parses class declaration with void method with single assignment expression statement with variable" $
      let baseName = "ClassDeclVoidMethodSingleAssignVar"
          fileName = mkFileName baseName
          srcSpanFun = makeSrcSpanAnn fileName
          assignSrcSpan = srcSpanFun 3 5 3 9
          varName1 = simpleName (srcSpanFun 3 5 3 5) "x" ExpName
          varName2 = simpleName (srcSpanFun 3 9 3 9) "y" ExpOrLockName
          assign = Assign assignSrcSpan (NameLhs varName1) (EqualA $ srcSpanFun 3 7 3 7) (NameExp varName2)
          ast = simpleClassDeclCompUnit (srcSpanFun 1 1 5 1)
                  (Id (srcSpanFun 1 7 1 7) "C")
                  (ClassBody (srcSpanFun 1 9 5 1)
                     [simpleMethodDecl (srcSpanFun 2 3 4 3)
                        (VoidType $ srcSpanFun 2 3 2 6)
                        (Id (srcSpanFun 2 8 2 8) "f")
                        []
                        (simpleMethodBody (srcSpanFun 2 12 4 3)
                           [BlockStmt $ ExpStmt (srcSpanFun 3 5 3 10) assign])])
      in successCase baseName ast

    it "parses class declaration with single field declaration with reference type" $
      let baseName = "ClassDeclSingleFieldRefType"
          fileName = mkFileName baseName
          srcSpanFun = makeSrcSpanAnn fileName
          ast = simpleClassDeclCompUnit (srcSpanFun 1 1 3 1)
                  (Id (srcSpanFun 1 7 1 7) "C")
                  (ClassBody (srcSpanFun 1 9 3 1)
                     [simpleFieldDecl (srcSpanFun 2 3 2 11)
                        (simpleRefType (srcSpanFun 2 3 2 8) "Object")
                        [simpleVarDecl (srcSpanFun 2 10 2 10) "x"]])
      in successCase baseName ast

    it "parses class declaration with single field declaration of primitive type with literal initializer" $
      let baseName = "ClassDeclSinglePrimFieldInit"
          fileName = mkFileName baseName
          srcSpanFun = makeSrcSpanAnn fileName
          ast = simpleClassDeclCompUnit (srcSpanFun 1 1 3 1)
                  (Id (srcSpanFun 1 7 1 7) "C")
                  (ClassBody (srcSpanFun 1 9 3 1)
                     [simpleFieldDecl (srcSpanFun 2 3 2 19)
                        (PrimType $ BooleanT $ srcSpanFun 2 3 2 9)
                        [simpleVarDeclInit (srcSpanFun 2 11 2 18)
                           (Id (srcSpanFun 2 11 2 11) "b")
                           (Lit $ Boolean (srcSpanFun 2 15 2 18) True)]])
      in successCase baseName ast

    it "parses class declaration with single field declaration of reference type with null initializer" $
      let baseName = "ClassDeclSingleRefFieldInit"
          fileName = mkFileName baseName
          srcSpanFun = makeSrcSpanAnn fileName
          ast = simpleClassDeclCompUnit (srcSpanFun 1 1 3 1)
                  (Id (srcSpanFun 1 7 1 7) "C")
                  (ClassBody (srcSpanFun 1 9 3 1)
                     [simpleFieldDecl (srcSpanFun 2 3 2 18)
                        (simpleRefType (srcSpanFun 2 3 2 8) "Object")
                        [simpleVarDeclInit (srcSpanFun 2 10 2 17)
                           (Id (srcSpanFun 2 10 2 10) "x")
                           (Lit $ Null $ srcSpanFun 2 14 2 17)]])
      in successCase baseName ast

    it "parses class declaration with multiple field declarations of primitive types with literal initializers" $
      let baseName = "ClassDeclMultPrimFieldsInit"
          fileName = mkFileName baseName
          srcSpanFun = makeSrcSpanAnn fileName
          ast = simpleClassDeclCompUnit (srcSpanFun 1 1 3 1)
                  (Id (srcSpanFun 1 7 1 7) "C")
                  (ClassBody (srcSpanFun 1 9 3 1)
                     [simpleFieldDecl (srcSpanFun 2 3 2 19)
                        (PrimType $ IntT $ srcSpanFun 2 3 2 5)
                        [ simpleVarDeclInit (srcSpanFun 2 7 2 11)
                            (Id (srcSpanFun 2 7 2 7) "x")
                            (Lit $ Int (srcSpanFun 2 11 2 11) 1)
                        , simpleVarDeclInit (srcSpanFun 2 14 2 18)
                            (Id (srcSpanFun 2 14 2 14) "y")
                            (Lit $ Int (srcSpanFun 2 18 2 18) 2)]])
      in successCase baseName ast

    it "parses class declaration with void method with single local variable declaration of reference type with null initializer" $
      let baseName = "ClassDeclVoidMethodSingleRefLocalVarInit"
          fileName = mkFileName baseName
          srcSpanFun = makeSrcSpanAnn fileName
          ast = simpleClassDeclCompUnit (srcSpanFun 1 1 5 1)
                  (Id (srcSpanFun 1 7 1 7) "C")
                  (ClassBody (srcSpanFun 1 9 5 1)
                     [simpleMethodDecl (srcSpanFun 2 3 4 3)
                        (VoidType $ srcSpanFun 2 3 2 6)
                        (Id (srcSpanFun 2 8 2 8) "f")
                        []
                        (simpleMethodBody (srcSpanFun 2 12 4 3)
                           [LocalVars (srcSpanFun 3 5 3 20)
                              []
                              (simpleRefType (srcSpanFun 3 5 3 10) "Object")
                              [simpleVarDeclInit (srcSpanFun 3 12 3 19)
                                 (Id (srcSpanFun 3 12 3 12) "x")
                                 (Lit $ Null $ srcSpanFun 3 16 3 19)]])])
      in successCase baseName ast

    it "parses class declaration with single low policy field" $
      let baseName = "ClassDeclSingleLowPolicyField"
          fileName = mkFileName baseName
          srcSpanFun = makeSrcSpanAnn fileName
          clVarTSrcSpan = srcSpanFun 2 31 2 36
          clVarTName = simpleName clVarTSrcSpan "Object" TypeName
          clVarType = ClassRefType (ClassType clVarTSrcSpan clVarTName [])
          ast = simpleClassDeclCompUnit (srcSpanFun 1 1 3 1)
                  (Id (srcSpanFun 1 7 1 7) "C")
                  (ClassBody (srcSpanFun 1 9 3 1)
                     [MemberDecl $
                        FieldDecl (srcSpanFun 2 3 2 43)
                          [Public $ srcSpanFun 2 3 2 8, Final $ srcSpanFun 2 10 2 14]
                          (PrimType $ PolicyT $ srcSpanFun 2 16 2 21)
                          [simpleVarDeclInit (srcSpanFun 2 23 2 42)
                             (Id (srcSpanFun 2 23 2 25) "low")
                             (PolicyExp $
                                PolicyLit (srcSpanFun 2 29 2 42)
                                  [Clause (srcSpanFun 2 31 2 40)
                                     []
                                     (ClauseDeclHead $
                                        ClauseVarDecl (srcSpanFun 2 31 2 38)
                                          clVarType
                                          (Id (srcSpanFun 2 38 2 38) "x"))
                                     []])]])
      in successCase baseName ast

    it "parses class declaration with single high policy field" $
      let baseName = "ClassDeclSingleHighPolicyField"
          fileName = mkFileName baseName
          srcSpanFun = makeSrcSpanAnn fileName
          ast = simpleClassDeclCompUnit (srcSpanFun 1 1 3 1)
                  (Id (srcSpanFun 1 7 1 7) "C")
                  (ClassBody (srcSpanFun 1 9 3 1)
                     [MemberDecl $
                        FieldDecl (srcSpanFun 2 3 2 33)
                          [Public $ srcSpanFun 2 3 2 8, Final $ srcSpanFun 2 10 2 14]
                          (PrimType $ PolicyT $ srcSpanFun 2 16 2 21)
                          [simpleVarDeclInit (srcSpanFun 2 23 2 32)
                             (Id (srcSpanFun 2 23 2 26) "high")
                             (PolicyExp $ PolicyLit (srcSpanFun 2 30 2 32) [])]])
      in successCase baseName ast

    it "parses class declaration with single field with reads policy modifier" $
      let baseName = "ClassDeclSingleFieldReadsMod"
          fileName = mkFileName baseName
          srcSpanFun = makeSrcSpanAnn fileName
          ast = simpleClassDeclCompUnit (srcSpanFun 1 1 3 1)
                  (Id (srcSpanFun 1 7 1 7) "C")
                  (ClassBody (srcSpanFun 1 9 3 1)
                     [MemberDecl $
                        FieldDecl (srcSpanFun 2 3 2 13)
                          [Reads (srcSpanFun 2 3 2 6)
                             (PolicyExp $ PolicyLit (srcSpanFun 2 4 2 6) [])]
                          (PrimType $ IntT $ srcSpanFun 2 8 2 10)
                          [simpleVarDecl (srcSpanFun 2 12 2 12) "x"]])
      in successCase baseName ast

    it "parses class declaration with single field with writes policy modifier" $
      let baseName = "ClassDeclSingleFieldWritesMod"
          fileName = mkFileName baseName
          srcSpanFun = makeSrcSpanAnn fileName
          ast = simpleClassDeclCompUnit (srcSpanFun 1 1 3 1)
                  (Id (srcSpanFun 1 7 1 7) "C")
                  (ClassBody (srcSpanFun 1 9 3 1)
                     [MemberDecl $
                        FieldDecl (srcSpanFun 2 3 2 13)
                          [Writes (srcSpanFun 2 3 2 6)
                             (PolicyExp $ PolicyLit (srcSpanFun 2 4 2 6) [])]
                          (PrimType $ IntT $ srcSpanFun 2 8 2 10)
                          [simpleVarDecl (srcSpanFun 2 12 2 12) "x"]])
      in successCase baseName ast

    it "parses class declaration with single method with one parameter" $
      let baseName = "ClassDeclSingleMethodOneParam"
          fileName = mkFileName baseName
          srcSpanFun = makeSrcSpanAnn fileName
          ast = simpleClassDeclCompUnit (srcSpanFun 1 1 4 1)
                  (Id (srcSpanFun 1 7 1 7) "C")
                  (ClassBody (srcSpanFun 1 9 4 1)
                     [(simpleMethodDecl (srcSpanFun 2 3 3 3)
                         (VoidType $ srcSpanFun 2 3 2 6)
                         (Id (srcSpanFun 2 8 2 8) "f")
                         [simpleFormalParam (srcSpanFun 2 10 2 14)
                            (PrimType $ IntT $ srcSpanFun 2 10 2 12)
                            (Id (srcSpanFun 2 14 2 14) "x")]
                         (simpleMethodBody (srcSpanFun 2 17 3 3) []))])
      in successCase baseName ast

    it "parses class declaration with single method with two parameters" $
      let baseName = "ClassDeclSingleMethodTwoParams"
          fileName = mkFileName baseName
          srcSpanFun = makeSrcSpanAnn fileName
          ast = simpleClassDeclCompUnit (srcSpanFun 1 1 4 1)
                  (Id (srcSpanFun 1 7 1 7) "C")
                  (ClassBody (srcSpanFun 1 9 4 1)
                     [(simpleMethodDecl (srcSpanFun 2 3 3 3)
                         (VoidType $ srcSpanFun 2 3 2 6)
                         (Id (srcSpanFun 2 8 2 8) "f")
                         [ simpleFormalParam (srcSpanFun 2 10 2 14)
                             (PrimType $ IntT $ srcSpanFun 2 10 2 12)
                             (Id (srcSpanFun 2 14 2 14) "x")
                         , simpleFormalParam (srcSpanFun 2 17 2 24)
                             (PrimType $ DoubleT $ srcSpanFun 2 17 2 22)
                             (Id (srcSpanFun 2 24 2 24) "y")]
                         (simpleMethodBody (srcSpanFun 2 27 3 3) []))])
      in successCase baseName ast

    it "parses class declaration with single method with varargs param" $
      let baseName = "ClassDeclSingleMethodVarArgsParam"
          fileName = mkFileName baseName
          srcSpanFun = makeSrcSpanAnn fileName
          ast = simpleClassDeclCompUnit (srcSpanFun 1 1 4 1)
                  (Id (srcSpanFun 1 7 1 7) "C")
                  (ClassBody (srcSpanFun 1 9 4 1)
                     [(simpleMethodDecl (srcSpanFun 2 3 3 3)
                         (VoidType $ srcSpanFun 2 3 2 6)
                         (Id (srcSpanFun 2 8 2 8) "f")
                         [ simpleFormalParam (srcSpanFun 2 10 2 14)
                             (PrimType $ IntT $ srcSpanFun 2 10 2 12)
                             (Id (srcSpanFun 2 14 2 14) "x")
                         , FormalParam (srcSpanFun 2 17 2 30)
                             []
                             (simpleRefType (srcSpanFun 2 17 2 22) "Object")
                             True
                             (Id (srcSpanFun 2 27 2 30) "objs")]
                         (simpleMethodBody (srcSpanFun 2 33 3 3) []))])
      in successCase baseName ast

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

      it "given a class declaration with single field declaration with missing name" $
        failureCase "ClassDeclSingleFieldMissName"

      it "given a class declaration with single field declaration with missing type" $
        failureCase "ClassDeclSingleFieldMissType"

      it "given a class declaration with single field declaration with missing semicolon" $
        failureCase "ClassDeclSingleFieldMissSemiColon"

      it "given a class declaration with single field declaration with modifier and missing type" $
        failureCase "ClassDeclSingleFieldModMissType"

      it "given a class declaration with single field and comma" $
        failureCase "ClassDeclSingleFieldComma"

      it "given a class declaration with multiple fields and missing comma" $
        failureCase "ClassDeclMultFieldsMissComma"

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

      -- Error message should be improved
      it "given a class declaration with void method with local variable declaration with missing semicolon" $
        failureCase "ClassDeclVoidMethodSingleLocalVarDeclMissSemiColon"

      -- Error message should be improved
      it "given a class declaration with void method with local variable declaration with missing name" $
        failureCase "ClassDeclVoidMethodSingleLocalVarDeclMissName"

      -- Error message should be improved
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

      it "given a class declaration with void method with assignment with modifier" $
        failureCase "ClassDeclVoidMethodAssignMod"

      it "given a class declaration with single field declaration with initializer with missing expression" $
        failureCase "ClassDeclSingleFieldInitMissExp"

      it "given a class declaration with single field declaration with initializer with missing semicolon" $
        failureCase "ClassDeclSingleFieldInitMissSemiColon"

      -- Error message should be improved
      it "given a class declaration with void method with single local variable declaration with initializer with missing semicolon" $
        failureCase "ClassDeclSingleLocalVarInitMissSemiColon"

      it "given a class declaration with single policy field with missing opening brace in initializer" $
        failureCase "ClassDeclSinglePolicyFieldMissOpenBrace"

      it "given a class declaration with single policy field with missing closing brace in initializer" $
        failureCase "ClassDeclSinglePolicyFieldMissCloseBrace"

      it "given a class declaration with single policy field with missing colon in empty initializer" $
        failureCase "ClassDeclSinglePolicyFieldEmptyMissColon"

      it "given a class declaration with single policy field with missing colon after clause head in initializer" $
        failureCase "ClassDeclSinglePolicyFieldClauseHeadMissColon"

      it "given a class declaration with single field with policy modifier with missing specifier symbol" $
        failureCase "ClassDeclSingleFieldPolicyModMissSymbol"

      it "given a class declaration with single method with parameter with missing type" $
        failureCase "ClassDeclSingleMethodParamMissType"

      it "given a class declaration with single method with parameter with missing name" $
        failureCase "ClassDeclSingleMethodParamMissName"

      it "given a class declaration with single method with two parameters with missing comma" $
        failureCase "ClassDeclSingleMethodTwoParamsMissComma"

      it "given a class declaration with single method with varargs param not at the last position" $
        failureCase "ClassDeclSingleMethodNotLastVarArgsParam"

-- Helper

makeSrcSpanAnn :: String -> Int -> Int -> Int -> Int -> Annotation
makeSrcSpanAnn fileName a b c d = srcSpanToAnn $ SrcSpan fileName a b c d

-- Infrastructure

successCase :: String -> AST -> IO ()
successCase baseName result = do
  input <- successRead baseName
  parse input (mkFileName baseName) `shouldBe` Right result

successRead :: String -> IO String
successRead baseName = readFile (successDir </> mkFileName baseName)

failureCase :: String -> IO ()
failureCase baseName = do
  (input, errMsg) <- failureRead baseName
  let Left err = parse input (mkFileName baseName)
  show err `shouldBe` errMsg

failureRead :: String -> IO (String, String)
failureRead baseName = liftM2 (,) (readFile (failureDir </> mkFileName baseName))
                                  (dropNewLine <$> readFile (failureDir </> baseName <.> "err"))
  where dropNewLine ""  = ""
        dropNewLine str = let l = last str
                          in if l == '\n' || l == '\r'
                               then dropNewLine (init str)
                               else str

mkFileName :: String -> String
mkFileName baseName = baseName <.> "para"

