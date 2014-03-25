-- | The resolvers follow the same order as the data definitions in @Syntax@ as
-- much as possible. The resolvers are split accordingly into separate modules
-- as is done for @Syntax@, i.e. Expressions, Modifiers, Names, Types and
-- Statements.
module Language.Java.Paragon.NameResolution.Resolvers
  (
    module Language.Java.Paragon.NameResolution.Resolvers.Names
  , rnClassType
  , rnTypeDecl
  ) where

import Control.Applicative ((<$>))
import Data.Traversable (mapM)
import Prelude hiding (mapM)

import Language.Java.Paragon.Error.StandardContexts
import Language.Java.Paragon.Error.StandardErrors
import Language.Java.Paragon.Monad.Base
import Language.Java.Paragon.Monad.NameRes
import Language.Java.Paragon.SrcPos
import Language.Java.Paragon.Syntax

import Language.Java.Paragon.NameResolution.Expansion
import Language.Java.Paragon.NameResolution.Resolvers.Expressions
import Language.Java.Paragon.NameResolution.Resolvers.Modifiers
import Language.Java.Paragon.NameResolution.Resolvers.Names
import Language.Java.Paragon.NameResolution.Resolvers.Types
import Language.Java.Paragon.NameResolution.Resolvers.Statements

-- | Resolve type declarations.
rnTypeDecl :: Resolve TypeDecl
rnTypeDecl (ClassTypeDecl classDecl) = do
  -- Typeparams not supported yet. add: extendExpansion (mkTpsExpn tps) $ do
  modifiers  <- mapM rnModifier (cdModifiers classDecl)
  superClass <- case cdSuperClass classDecl of
                  Nothing -> return Nothing
                  Just s  -> Just <$> rnClassType s
  interfaces <- mapM rnClassType (cdInterfaces classDecl)
  withErrCtxt (classBodyContext classDecl) $ do
    body       <- rnClassBody (cdBody classDecl)
    return $ ClassTypeDecl $ classDecl {
        cdModifiers  = modifiers
      , cdSuperClass = superClass
      , cdInterfaces = interfaces
      , cdBody       = body
      }
rnTypeDecl (InterfaceTypeDecl intDecl) = do
  modifiers  <- mapM rnModifier (intdModifiers intDecl)
  interfaces <- mapM rnClassType (intdInterfaces intDecl)
  body       <- rnInterfaceBody (intdBody intDecl)
  return $ InterfaceTypeDecl $ intDecl {
      intdModifiers  = modifiers
    , intdInterfaces = interfaces
    , intdBody       = body
    }

-- | Resolves a class type by simply calling the resolvers on its name and
-- type arguments. TODO: type arguments not yet supported.
rnClassType :: Resolve ClassType
rnClassType ct = do
  n'   <- rnName (ctName ct)
  return $ ct {ctName = n' }

-- | Interface declarations are not supported yet.
rnInterfaceBody :: Resolve InterfaceBody
rnInterfaceBody i = failEC i $ unsupportedError "interface body" defaultSpan

-- | Extends the expansion to include all fields and method defined in the class
-- body, then continues name resolving on the class body itself.
rnClassBody :: Resolve ClassBody
rnClassBody classBody = do 
  let fieldNames  = map varDeclId $ concat [ fieldDeclVarDecls f |
                      MemberDecl f@(FieldDecl {}) <- (cbDecls classBody) ]
  let methodNames = [ methodDeclId m |
                      MemberDecl m@(MethodDecl {}) <- (cbDecls classBody) ]
  let expns = expansionUnion $ map mkExpExpansion (map idIdent fieldNames) ++
                               map mkMethodExpansion (map idIdent methodNames)
  extendExpansion expns $ do
    decls <- mapM rnClassBodyDecl (cbDecls classBody)
    return $ classBody { cbDecls = decls }

-- | Resolve all declarations in the class body.
rnClassBodyDecl :: Resolve ClassBodyDecl
rnClassBodyDecl (MemberDecl memberDecl) = do
  newMDecl <- rnMemberDecl memberDecl
  return $ MemberDecl newMDecl

-- | Resolve member declaration.
rnMemberDecl :: Resolve MemberDecl
rnMemberDecl fieldDecl@(FieldDecl {}) = do
  modifiers <- mapM rnModifier (fieldDeclModifiers fieldDecl)
  ty        <- rnType (fieldDeclType fieldDecl)
  vars      <- mapM rnVarDecl (fieldDeclVarDecls fieldDecl)
  return $ fieldDecl { fieldDeclModifiers = modifiers
                     , fieldDeclType      = ty
                     , fieldDeclVarDecls  = vars
                     }
rnMemberDecl methodDecl@(MethodDecl {}) = do
  modifiers <- mapM rnModifier (methodDeclModifiers methodDecl)
  retTy     <- rnReturnType (methodDeclReturnType methodDecl)
  formParam <- mapM rnFormalParam (methodDeclFormalParams methodDecl)
  body      <- rnMethodBody (methodDeclBody methodDecl)
  return $ methodDecl { methodDeclModifiers    = modifiers
                      , methodDeclReturnType   = retTy
                      , methodDeclFormalParams = formParam
                      , methodDeclBody         = body
                      }

-- | Resolve variable/field declaration.
rnVarDecl :: Resolve VarDecl
rnVarDecl varDecl = do
  varInit <- mapM rnVarInit (varDeclInit varDecl)
  return $ varDecl { varDeclInit = varInit }

-- | Resolve formal parameter.
rnFormalParam :: Resolve FormalParam
rnFormalParam formalParam = do
  modifiers <- mapM rnModifier (formalParamModifiers formalParam)
  ty        <- rnType (formalParamType formalParam)
  return $ formalParam { formalParamModifiers = modifiers
                       , formalParamType      = ty
                       }

-- | Resolve optional method body.
rnMethodBody :: Resolve MethodBody
rnMethodBody methodBody = do
  block <- mapM rnBlock (methodBodyBlock methodBody)
  return $ methodBody { methodBodyBlock = block }

-- | Resolve the explicit initialiser for field/variable declaration.
rnVarInit :: Resolve VarInit
rnVarInit varInit = do
  initExpr <- rnExp (varInitExp varInit)
  return $ varInit { varInitExp = initExpr }

-- | Resolve a code block.
rnBlock :: Resolve Block
rnBlock block = do
  blockS <- rnBlockStmts (blockAnnStmts block)
  return $ block { blockAnnStmts = blockS }

-- | Resolve a list of block statements. Using special function instead of mapM,
-- since possible declarations need to extend the expansion.
rnBlockStmts :: [BlockStmt SrcSpan] -> NameRes [BlockStmt SrcSpan]
rnBlockStmts [] = return []
rnBlockStmts (bs:bss) = do
  (bs', bss') <- rnBlockStmt bs $ rnBlockStmts bss
  return $ bs':bss'

-- | Resolve a single block statement. In case the statement is a local variable
-- declaration, extend the expansion with this declaration(s) in @rnVarDecls@.
rnBlockStmt :: BlockStmt SrcSpan -> NameRes a -> NameRes (BlockStmt SrcSpan, a)
rnBlockStmt (BlockStmt stmt) cont = do
  stmt' <- rnStmt stmt
  cont' <- cont
  return (BlockStmt stmt', cont')  
rnBlockStmt localVars@(LocalVars {}) cont = do
  modifiers <- mapM rnModifier (localVarsModifiers localVars)
  ty        <- rnType (localVarsType localVars)
  (vds, a)  <- rnVarDecls (localVarsDecls localVars) cont
  return $ ( localVars { localVarsModifiers = modifiers
                       , localVarsType      = ty
                       , localVarsDecls     = vds
                       }
           , a)

-- | Resolve a list of variable declarations. Makes a call to @rnVarDecl@ and
-- extends the current expansion for the provided continuation, i.e. these
-- variable declarations can be found in the expansion for the remaining name
-- resolution.
rnVarDecls :: [VarDecl SrcSpan] -> NameRes a -> NameRes ([VarDecl SrcSpan], a)
rnVarDecls = rnVarDeclsAcc []
 where rnVarDeclsAcc :: [VarDecl SrcSpan]    -- ^ Accumulator (reversed)
                     -> [VarDecl SrcSpan]    -- ^ List to resolve
                     -> NameRes a            -- ^ Continuation
                     -> NameRes ([VarDecl SrcSpan], a) -- ^ Result (re-reversed)
       rnVarDeclsAcc acc [] cont = ((,) (reverse acc)) <$> cont
       rnVarDeclsAcc acc (vd@(VarDecl _ i _) : vds) cont = do
         let expn = mkExpExpansion $ idIdent i
         extendExpansion expn $ do vd' <- rnVarDecl vd
                                   rnVarDeclsAcc (vd':acc) vds cont
