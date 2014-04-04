module Language.Java.Paragon.TypeChecker
  ( -- * Type checking phase
    typeCheck
  ) where

import Control.Applicative ((<$>))
import Control.Monad (when)

import Language.Java.Paragon.Annotation
import Language.Java.Paragon.Error.StandardContexts
import Language.Java.Paragon.Interaction
import Language.Java.Paragon.Monad.Base
import Language.Java.Paragon.Monad.PiReader
import Language.Java.Paragon.SrcPos
import Language.Java.Paragon.Syntax

import Language.Java.Paragon.TypeChecker.Errors
import Language.Java.Paragon.TypeChecker.Instantiate
import Language.Java.Paragon.TypeChecker.Monad.TcSignatureM
import Language.Java.Paragon.TypeChecker.PkgMap
import Language.Java.Paragon.TypeChecker.TcTypes
import Language.Java.Paragon.TypeChecker.TypeMap

thisModule :: String
thisModule = "Language.Java.Paragon.TypeChecker"

-- | Type checking phase. Returns an AST with type annotation for nodes that
-- have a type. The base name of the file containing the class of this AST is
-- required to check that the right type is defined in this .para file.
typeCheck :: PiPath     -- ^ Directories where .pi files can be found.
          -> String     -- ^ Base name of the file.
          -> AST        -- ^ AST from previous phase.
          -> BaseM AST  -- ^ AST after type checking phase.
typeCheck piPath baseName ast = withErrCtxt (compPhaseContext "Type Checking") $ do
  when ((length $ cuTypeDecls ast) /= 1) $
    panic (thisModule ++ ".typeCheck") $ "Encountered multiple / zero type " ++
      "declarations in one file. This should not occur in this phase."
  let [typeDecl] = cuTypeDecls ast  
  -- 1. Create skolem types for the type parameters (generics).
  let (typeParamSubst, skClassType) = createSkolemSubst typeDecl
  -- 2. Apply the skolemisation on the type declaration.
  let skolemTypeDecl = instantiate typeParamSubst typeDecl
  liftToBaseM piPath $ runTcSignatureM skClassType $ do
    -- 3. Get the package name.
    let maybePkgDecl = fmap pdName (cuPkgDecl ast)
    -- 4. Type check type declaration.
    tcTypeDecl <- typeCheckTypeDecl baseName maybePkgDecl skolemTypeDecl
    -- 5. Packages and import declarations have no type.
    let tcPkgDecl = fmap (\x -> x { pdAnn = (pdAnn x) { annType = Nothing } } ) (cuPkgDecl ast)
    let tcImpDecls = map (\x -> x { impdAnn = (impdAnn x) { annType = Nothing } } ) (cuImportDecls ast)
    -- 6. Return updated AST.
    return $ ast { cuPkgDecl     = tcPkgDecl
                 , cuImportDecls = tcImpDecls
                 , cuTypeDecls   = [tcTypeDecl]
                 }

-- | Create a mapping from type parameter to skolemised type to be used in type
-- checking.
createSkolemSubst :: TypeDecl -> ([(TypeParam, TcTypeParam)], TcClassType)
createSkolemSubst (ClassTypeDecl classDecl) =
  let typeParams   = cdTypeParams classDecl
      skTypeParams = map skolemiseParam typeParams
      skSubst      = zip typeParams skTypeParams
      skClassType  = TcClassType (typeName [cdId classDecl]) skTypeParams
  in (skSubst, skClassType)
createSkolemSubst (InterfaceTypeDecl interfaceDecl) =
  let typeParams   = intdTypeParams interfaceDecl
      skTypeParams = map skolemiseParam typeParams
      skSubst      = zip typeParams skTypeParams
      skClassType  = TcClassType (typeName [intdId interfaceDecl]) skTypeParams
  in (skSubst, skClassType)

-- | Create a skolem type for the provided type parameter.
skolemiseParam :: TypeParam -> TcTypeParam
skolemiseParam _ = notImplemented (thisModule ++ ".skolemiseParam")

-- Type checking functions.

-- | Type checking type declaration.
typeCheckTypeDecl :: String -> Maybe Name -> TcSignature TypeDecl
typeCheckTypeDecl baseName mPkg (ClassTypeDecl classDecl) =
  ClassTypeDecl <$> typeCheckClassDecl baseName mPkg classDecl
typeCheckTypeDecl baseName mPkg (InterfaceTypeDecl interfaceDecl) =
  InterfaceTypeDecl <$> typeCheckInterfaceDecl baseName mPkg interfaceDecl

-- | Type checking interface declaration. Not implemented yet.
typeCheckInterfaceDecl :: String -> Maybe Name -> TcSignature InterfaceDecl
typeCheckInterfaceDecl _ _ _ = notImplemented (thisModule ++ ".typeCheckInterfaceDecl")

-- Type checking class declaration.
typeCheckClassDecl :: String -> Maybe Name -> TcSignature ClassDecl
typeCheckClassDecl baseName mPkg classDecl = do
  finePrint $ "Entering: " ++ thisModule ++ ".typeCheckClassDecl"
  withErrCtxt (classBodyContext classDecl) $ do
    check ((idIdent $ cdId classDecl) == baseName) $
      fileNameMismatch baseName classDecl (cdId classDecl)
    let _memberDecls = [ m | MemberDecl m <- cbDecls $ cdBody classDecl ]
        idFunc     = map (Id (srcSpanToAnn defaultSpan))
        objectName = combineNames [ pkgName $ idFunc ["java", "lang"]
                                  , typeName $ idFunc ["Object"] ]
        objectType = ClassType { ctAnn      = srcSpanToAnn defaultSpan
                               , ctName     = objectName
                               , ctTypeArgs = []
                               }
        superTypes  = maybe [objectType] (:[]) (cdSuperClass classDecl)
    registerThisType mPkg (cdId classDecl) superTypes
    return classDecl

-- | Add this type to the global package map.
registerThisType :: Maybe Name   -- ^ Optional package name.
                 -> Id           -- ^ Type declaration's identifier.
                 -> [ClassType]  -- ^ Super types (incl. interfaces).
                 -> TcSignatureM ()
registerThisType mPkg tdId superTypes = do
    let tcSuperTypes = map 
          (\c -> TcClassType (ctName c) 
                             (panic (typeCheckerBase ++ ".registerThisType") $ "Type parameters not supported yet.")) 
          superTypes
    -- TODO: evaluate super types and add their members to this declaration's
    -- type map.
    let fullN = Name { nameAnn  = srcSpanToAnn defaultSpan
                     , nameId   = tdId
                     , nameType = TypeName
                     , namePrefix = mPkg 
                     }
        thisSig = TypeSignature { tsType         = TcClassRefType $ TcClassType fullN []
                                , tsIsClass      = True
                                , tsIsFinal      = False
                                , tsSuperClasses = tcSuperTypes
                                , tsInterfaces   = []
                                -- Members not yet needed at this-registration:
                                , tsMembers      = emptyTypeMap 
                                }
    modifyPkgMap $ insertPkgMapType fullN thisSig
