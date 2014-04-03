module Language.Java.Paragon.TypeChecker
  ( -- * Type checking phase
    typeCheck
  ) where

import Control.Monad (when)

import Language.Java.Paragon.Annotation
import Language.Java.Paragon.Interaction
import Language.Java.Paragon.Monad.Base
import Language.Java.Paragon.Monad.PiReader
import Language.Java.Paragon.Syntax

import Language.Java.Paragon.TypeChecker.Monad.TcSignatureM
import Language.Java.Paragon.TypeChecker.Types

thisModule :: String
thisModule = "Language.Java.Paragon.TypeChecker"

-- | Type checking phase. Returns an AST with type annotation for nodes that
-- have a type. The base name of the file containing the class of this AST is
-- required to check that the right type is defined in this .para file.
typeCheck :: PiPath     -- ^ Directories where .pi files can be found.
          -> String     -- ^ Base name of the file.
          -> AST        -- ^ AST from previous phase.
          -> BaseM AST  -- ^ AST after type checking phase.
typeCheck piPath baseName ast = do
  when ((length $ cuTypeDecls ast) /= 1) $
    panic (thisModule ++ ".typeCheck") $ "Encountered multiple / zero type " ++
      "declarations in one file. This should not occur in this phase."
  let [typeDecl] = cuTypeDecls ast  
  -- 1. Create skolem types for the type parameters (generics).
  let typeParamSubst = createSkolemSubst typeDecl
  -- 2. Apply the skolemisation on the type declaration.
  -- let skolemTypeDecl = instantiate typeParamSubst typeDecl
  liftToBaseM piPath $ runTcSignatureM typeDecl $ do
    -- 2. Get the package name.
    let maybePkgDecl = fmap pdName (cuPkgDecl ast)
    -- 3. Type check type declaration.
    tcTypeDecl <- typeCheckTypeDecl baseName maybePkgDecl typeDecl
    -- 4. Packages and import declarations have no type.
    let tcPkgDecl = fmap (\x -> x { pdAnn = (pdAnn x) { annType = Nothing } } ) (cuPkgDecl ast)
    let tcImpDecls = map (\x -> x { impdAnn = (impdAnn x) { annType = Nothing } } ) (cuImportDecls ast)
    -- 5. Return updated AST.
    return $ ast { cuPkgDecl     = tcPkgDecl
                 , cuImportDecls = tcImpDecls
                 , cuTypeDecls   = [tcTypeDecl]
                 }

typeCheckTypeDecl = undefined

-- | Create a mapping from type parameter to skolemised type to be used in type
-- checking.
createSkolemSubst :: TypeDecl -> [(TypeParam, TcType)]
createSkolemSubst (ClassTypeDecl classDecl) =
  let typeParams = cdTypeParams classDecl
  in zip typeParams (map skolemiseParam typeParams)
createSkolemSubst (InterfaceTypeDecl interfaceDecl) =
  let typeParams = intdTypeParams interfaceDecl
  in zip typeParams (map skolemiseParam typeParams)

-- | Create a skolem type for the provided type parameter.
skolemiseParam :: TypeParam -> TcType
skolemiseParam _ = panic (thisModule ++ ".skolemiseParam") $
  "This function is not implemented yet."
