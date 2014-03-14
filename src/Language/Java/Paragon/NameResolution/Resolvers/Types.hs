module Language.Java.Paragon.NameResolution.Resolvers.Types
  (
    -- * Resolvers
    rnClassType
  , rnTypeDecl
  ) where

import Language.Java.Paragon.Monad.NameRes
import Language.Java.Paragon.Syntax

-- TODO:
-- | Resolves a class type by simply calling the resolvers on its name and
-- type arguments. TODO: type arguments not yet supported.
rnClassType :: Resolve ClassType
rnClassType = undefined

-- | 
rnTypeDecl :: Resolve TypeDecl
rnTypeDecl = undefined
{-
rnTypeDecl (ClassTypeDecl pos (ClassDecl pos' ms ci tps mSuper impls cb)) = do
    extendExpansion (mkTpsExpn tps) $
      ClassTypeDecl pos <$> 
          (ClassDecl pos'
              <$> mapM rnModifier ms
              <*> pure ci
              <*> mapM rnTypeParam tps -- relevant because of wildcards
              <*> mapM rnClassType mSuper
              <*> mapM rnClassType impls
              <*> rnClassBody cb)
rnTypeDecl (InterfaceTypeDecl pos (InterfaceDecl pos' ms ii tps supers ib)) = do
    extendExpansion (mkTpsExpn tps) $
      InterfaceTypeDecl pos <$>
          (InterfaceDecl pos'
              <$> mapM rnModifier ms
              <*> pure ii
              <*> mapM rnTypeParam tps -- relevant because of wildcards
              <*> mapM rnClassType supers
              <*> rnInterfaceBody ib)
rnTypeDecl _ = failE . mkErrorFromInfo $
                 UnsupportedFeature "Enum declarations not yet supported"
-}
