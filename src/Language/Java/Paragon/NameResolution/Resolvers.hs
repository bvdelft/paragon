module Language.Java.Paragon.NameResolution.Resolvers
  (
    module Language.Java.Paragon.NameResolution.Resolvers.Names
  , rnClassType
  , rnTypeDecl
  ) where

import Control.Applicative ((<$>))

import Language.Java.Paragon.Error.StandardErrors
import Language.Java.Paragon.Monad.Base
import Language.Java.Paragon.Monad.NameRes
import Language.Java.Paragon.SrcPos
import Language.Java.Paragon.Syntax

import Language.Java.Paragon.NameResolution.Resolvers.Names


-- | Resolves a class type by simply calling the resolvers on its name and
-- type arguments. TODO: type arguments not yet supported.
rnClassType :: Resolve ClassType
rnClassType ct = do
  n'   <- rnName (ctName ct)
  return $ ct {ctName = n' }

-- | Resolve type declarations.
rnTypeDecl :: Resolve TypeDecl
rnTypeDecl (ClassTypeDecl classDecl) = do
  -- Typeparams not supported yet. add: extendExpansion (mkTpsExpn tps) $ do
  modifiers  <- mapM rnModifiers (cdModifiers classDecl)
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
rnTypeDecl i@(InterfaceTypeDecl _) = 
  failEC i $ unsupportedError "interface types" defaultSpan

rnModifiers :: Resolve Modifier
rnModifiers = undefined

rnClassBody :: Resolve ClassBody
rnClassBody = undefined
