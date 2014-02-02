-- | Java code generator. Performs transformations an AST.
module Language.Java.Paragon.CodeGenJava
  (
    generateJavaCode
  ) where

import Language.Java.Paragon.Syntax
import Language.Java.Paragon.SrcPos

-- | Main code generation type class.
-- Denotes how code for a particular syntax tree node is generated.
class CodeGen ast where
  -- TODO: change type signature for correct annotation types in the future
  genCode :: ast SrcSpan -> ast ()

generateJavaCode :: AST SrcSpan -> AST ()
generateJavaCode = genCode

instance CodeGen CompilationUnit where
  genCode = fmap (const ())

