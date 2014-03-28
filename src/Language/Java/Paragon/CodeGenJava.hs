-- | Java code generator. Performs transformations an AST.
module Language.Java.Paragon.CodeGenJava
  (
    generateJavaCode
  ) where

import Language.Java.Paragon.Syntax

-- | Main code generation type class.
-- Denotes how code for a particular syntax tree node is generated.
class CodeGen ast where
  genCode :: ast -> ast

generateJavaCode :: AST -> AST
generateJavaCode = genCode

instance CodeGen CompilationUnit where
  genCode = id

