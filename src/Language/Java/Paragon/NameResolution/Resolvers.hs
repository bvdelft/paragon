-- | The resolvers follow the same order as the data definitions in @Syntax@ as
-- much as possible.
module Language.Java.Paragon.NameResolution.Resolvers
  (
    module Language.Java.Paragon.NameResolution.Resolvers.Names
  , rnClassType
  , rnTypeDecl
  ) where

import Control.Applicative ((<$>))
import Data.Traversable (mapM)
import Prelude hiding (mapM)

import Language.Java.Paragon.Error.StandardErrors
import Language.Java.Paragon.Monad.Base
import Language.Java.Paragon.Monad.NameRes
import Language.Java.Paragon.SrcPos
import Language.Java.Paragon.Syntax

import Language.Java.Paragon.NameResolution.Expansion
import Language.Java.Paragon.NameResolution.Resolvers.Expressions
import Language.Java.Paragon.NameResolution.Resolvers.Names
import Language.Java.Paragon.NameResolution.Resolvers.Types
import Language.Java.Paragon.NameResolution.Resolvers.Statements

-- | Resolves a class type by simply calling the resolvers on its name and
-- type arguments. TODO: type arguments not yet supported.
rnClassType :: Resolve ClassType
rnClassType classType = do
  name <- rnName (mkNameSrcSpan TypeName $ map fst $ ctIdsAndTypeArgs classType)
  return $ classType { ctIdsAndTypeArgs = zip (flattenName name) (map snd $ ctIdsAndTypeArgs classType) }

-- | Resolve type declarations.
rnTypeDecl :: Resolve TypeDecl
rnTypeDecl (ClassTypeDecl classDecl) = do
  -- Typeparams not supported yet. add: extendExpansion (mkTpsExpn tps) $ do
  modifiers  <- mapM rnModifier (cdModifiers classDecl)
  superClass <- case cdSuperClass classDecl of
                  Nothing -> return Nothing
                  Just s  -> Just <$> rnClassType s
  interfaces <- mapM rnClassType (cdInterfaces classDecl)
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

rnInterfaceBody :: Resolve InterfaceBody
rnInterfaceBody i = failEC i $ unsupportedError "non-empty interfaces" defaultSpan

-- | Resolve modifiers - in particular, Paragon-specific modifiers.
rnModifier :: Resolve Modifier
rnModifier modifier = return modifier -- only for Paragon-modifiers

-- | Extends the expansion to include all fields and method defined in the class
-- body, then continues name resolving on the class body itself.
rnClassBody :: Resolve ClassBody
rnClassBody classBody = do 
  let fieldNames  = map varDeclId $ concat [ fieldDeclVarDecls f | MemberDecl f@(FieldDecl {}) <- (cbDecls classBody) ]
  let methodNames = [ methodDeclId m | MemberDecl m@(MethodDecl {}) <- (cbDecls classBody) ]
  let expns = expansionUnion $ map mkExpExpansion (map idIdent fieldNames) ++
                               map mkMethodExpansion (map idIdent methodNames)
  extendExpansion expns $ do
    decls <- mapM rnClassBodyDecl (cbDecls classBody)
    return $ classBody { cbDecls = decls }

-- | Resolve all declarations in the class body
rnClassBodyDecl :: Resolve ClassBodyDecl
rnClassBodyDecl (MemberDecl memberDecl) = do
  newMDecl <- rnMemberDecl memberDecl
  return $ MemberDecl newMDecl

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

rnVarDecl :: Resolve VarDecl
rnVarDecl varDecl = do
  varInit <- mapM rnVarInit (varDeclInit varDecl)
  return $ varDecl { varDeclInit = varInit }

rnFormalParam :: Resolve FormalParam
rnFormalParam formalParam = do
  modifiers <- mapM rnModifier (formalParamModifiers formalParam)
  ty        <- rnType (formalParamType formalParam)
  return $ formalParam { formalParamModifiers = modifiers
                       , formalParamType      = ty
                       }

rnMethodBody :: Resolve MethodBody
rnMethodBody methodBody = do
  block <- mapM rnBlock (methodBodyBlock methodBody)
  return $ methodBody { methodBodyBlock = block }

rnVarInit :: Resolve VarInit
rnVarInit varInit = do
  initExpr <- rnExp (varInitExp varInit)
  return $ varInit { varInitExp = initExpr }

rnBlock :: Resolve Block
rnBlock block = do
  blockS <- rnBlockStmts (blockAnnStmts block)
  return $ block { blockAnnStmts = blockS }

-- | Special function instead of mapM, since declarations need to extend the
-- expansion.
rnBlockStmts :: [BlockStmt SrcSpan] -> NameRes [BlockStmt SrcSpan]
rnBlockStmts [] = return []
rnBlockStmts (bs:bss) = do
  (bs', bss') <- rnBlockStmt bs $ rnBlockStmts bss
  return $ bs':bss'

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
