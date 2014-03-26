module Language.Java.Paragon.ASTHelpers
  where

import Language.Java.Paragon.Error.StandardContexts

import Language.Java.Paragon.Error
import Language.Java.Paragon.Syntax
import Language.Java.Paragon.SrcPos

-- Some short-hands for creating error contexts, with undefined/panic messages
-- for locations that the error context is assumed not to address.

defClassBodyContext :: String -> ErrorContext
defClassBodyContext name =
  let cId   = Id defaultSpan name
      u     = undefined
      cDecl = ClassDecl u u cId u u u u
  in classBodyContext cDecl

defMethodContext :: String -> ErrorContext
defMethodContext name =
  let mId   = Id defaultSpan name
      u     = undefined
      mDecl = MethodDecl u u u u mId u u
  in memberDeclContext mDecl

-- Helpers for modifying AST. Some assumption here, e.g. only altering RefType,
-- not PrimType.

-- | A modifier of the AST selects a certain element of type 'a' in the AST-node
-- 'b' and applies the provided function on it.
type ASTModifier a b = (a -> a) -- ^ Modifies an element in b
                     -> b       -- ^ The larger AST-node to be modified
                     -> b       -- ^ The modified AST.

-- | Modify the i-th declaration in the body. Assumes there is only one
-- classTypeDecl.
modifyBodyDecl :: Int -> ASTModifier (ClassBodyDecl a) (AST a)
modifyBodyDecl i f ast =
  let (ClassTypeDecl classDecl) = head $ cuTypeDecls ast
      classBody = cdBody classDecl
      (xs,y:ys) = splitAt i (cbDecls classBody)
      newDecls  = xs ++ (f y:ys)
      newBody   = classBody { cbDecls = newDecls }
  in ast { cuTypeDecls = [ClassTypeDecl classDecl { cdBody = newBody} ] }

modifyRefTypePrefix :: ASTModifier (Maybe (Name a)) (RefType a)
modifyRefTypePrefix f (ClassRefType ct) =
  let name    = ctName ct
      newName = name { namePrefix = f (namePrefix name) }
  in ClassRefType ct { ctName = newName }

modifyFieldDeclType :: ASTModifier (RefType a) (ClassBodyDecl a)
modifyFieldDeclType f (MemberDecl fieldDecl) = 
  let (RefType rt) = fieldDeclType fieldDecl
      rt' = f rt
  in MemberDecl fieldDecl { fieldDeclType = RefType rt' }

modifyLocalVarsType :: ASTModifier (RefType a) (BlockStmt a)
modifyLocalVarsType f localVars =
  let (RefType rt) = localVarsType localVars
  in localVars { localVarsType = RefType $ f rt }

-- | Modify the i-th statement in this method
modifyMethodBlockStmt :: Int -> ASTModifier (BlockStmt a) (ClassBodyDecl a)
modifyMethodBlockStmt i f (MemberDecl methodDecl) =
  let body = methodDeclBody methodDecl
      bodyBlock = methodBodyBlock body
  in case bodyBlock of
       Just block -> let (xs,y:ys) = splitAt i (blockAnnStmts block)
                         newBlock  = block { blockAnnStmts = xs ++ (f y:ys) }
                     in (MemberDecl methodDecl { methodDeclBody =
                           body { methodBodyBlock = Just newBlock } } )
       Nothing    -> (MemberDecl methodDecl) 

modifyFieldDeclInitExp :: Int -> ASTModifier (Exp a) (ClassBodyDecl a)
modifyFieldDeclInitExp i f (MemberDecl fieldDecl) = 
  let (xs,y:ys) = splitAt i (fieldDeclVarDecls fieldDecl)
      newInit   = fmap (\x -> x { varInitExp = f (varInitExp x) }) (varDeclInit y)
      newVarD   = xs ++ (y { varDeclInit = newInit }:ys)
  in MemberDecl fieldDecl { fieldDeclVarDecls = newVarD }

modifyPolicyClause :: Int -> ASTModifier (Clause a) (Exp a)
modifyPolicyClause i f (PolicyExp polExp) =
  let (xs,y:ys) = splitAt i (policyClauses polExp)
  in PolicyExp polExp { policyClauses = xs ++ (f y:ys) }
modifyPolicyClause _ _ _ = error "Incorrect call by test: modifyPolicyClause"

modifyDeclHeadRef :: ASTModifier (RefType a) (Clause a)
modifyDeclHeadRef f clause =
  let (ClauseDeclHead hd) = clauseHead clause
      nt = f (clauseVarDeclType hd)
  in clause { clauseHead = ClauseDeclHead hd { clauseVarDeclType = nt } }
