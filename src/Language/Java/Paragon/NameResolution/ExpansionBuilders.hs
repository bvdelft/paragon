-- | This module provides helper functionality for the name resolution stage in
-- compilation. The functions included here build expansion maps.
module Language.Java.Paragon.NameResolution.ExpansionBuilders
  (
    -- * Builders
    buildMapFromImportName
  , buildMapFromImports
  , buildMapFromPiPath
  , buildMapFromPkg
  , buildMapFromTd
  ) where

import Control.Monad (when)
import Data.List (nub)

import Language.Java.Paragon.Error.StandardErrors
import Language.Java.Paragon.Interaction
import Language.Java.Paragon.Monad.Base
import Language.Java.Paragon.Monad.PiReader
import Language.Java.Paragon.Monad.NameRes
import Language.Java.Paragon.Syntax
import Language.Java.Paragon.SrcPos

import Language.Java.Paragon.NameResolution.Errors
import Language.Java.Paragon.NameResolution.Expansion
import Language.Java.Paragon.NameResolution.ExpansionRecord
import Language.Java.Paragon.NameResolution.Resolvers
import Language.Java.Paragon.NameResolution.Helpers

thisModule :: String
thisModule = libraryBase ++ ".NameResolution.ExpansionBuilders"

-- | Build expansion map from an import declaration
-- Returns declaration with resolved prefix and the constructed expansion.
-- Creates error when encountering an unsupported import, i.e. a static
-- import; the phase is continued with an empty expansion mapping for the
-- provided import.
buildMapFromImportName :: ImportDecl SrcSpan
                       -> PiReader (ImportDecl SrcSpan, Expansion)
buildMapFromImportName (SingleTypeImport srcspan name) = do
  -- eg: import java.util.Vector;
  finePrint $ "Building expansion map for single import: " ++ unparsePrint name
  mPre <- resolvePrefix (namePrefix name) -- eg: java.util
  let resName = name {nameType = TypeName, namePrefix = mPre }
  let resImp  = SingleTypeImport srcspan resName
  let resExpn = mkTypeExpansionWithPrefix mPre (idIdent (nameId resName))
  isTy <- doesTypeExist resName
  if isTy
   then return $ (resImp, resExpn)
   else failEC (resImp, emptyExpansion) $ 
     unresolvedName resName srcspan
  
buildMapFromImportName (TypeImportOnDemand srcspan name) = do
  -- eg: import java.util.*;
  finePrint $ "Building expansion map for onDemand import: " ++ 
              unparsePrint name
  mPre <- resolvePrefix (namePrefix name) -- eg: java
  let resName = name { namePrefix = mPre }
  let resImp  = TypeImportOnDemand srcspan resName
  case nameType name of
    PkgName       -> do 
      isPkg <- doesPkgExist resName
      if isPkg
       then do 
         piIds <- getPkgContents resName
         return (resImp, 
           expansionUnion $ 
             map (mkTypeExpansionWithPrefix (Just resName)) piIds)
       else failEC (resImp, emptyExpansion) $
         unresolvedName resName srcspan
    TypeName      -> do
      failEC (resImp, emptyExpansion) $ unsupportedError "inner types" srcspan
    PkgOrTypeName -> do 
      isType <- doesTypeExist name
      isPkg  <- doesPkgExist  name
      if isType
       then buildMapFromImportName
              (TypeImportOnDemand srcspan name { nameType = TypeName })
       else if isPkg
         then buildMapFromImportName
                (TypeImportOnDemand srcspan name { nameType = PkgName })
         else failEC (resImp, emptyExpansion) $
                invalidPrefix (unparsePrint name) (nameAnn name)
    other         -> panic (thisModule ++ ".buildMapFromImportName") $
      "Unexpected name " ++ show name ++ " of nametype " ++ show other
  
buildMapFromImportName other = do
  finePrint $ "Throwing error for static import: " ++ show other
  failEC (other, emptyExpansion) $ 
    unsupportedError "static imports" (impdAnn other)



-- | Build an expansion map from explicit (or implicit) imports
-- (or, incidentally, for implicit import of local package).
buildMapFromImports :: [ImportDecl SrcSpan] 
                    -> PiReader ([ImportDecl SrcSpan], Expansion)
buildMapFromImports imps = do
  (imps', expns) <- fmap unzip $ mapM buildMapFromImportName imps
  return (imps', expansionUnion expns)


-- | Return expansion map that contains all the types and packages
-- in the pi path
buildMapFromPiPath :: PiReader Expansion
buildMapFromPiPath = do
  (tys,pkgs) <- getPiPathContents
  return $ expansionUnion $ map mkPkgExpansion  pkgs ++
                            map mkTypeExpansion tys
         
-- | Build an expansion map from a package declaration
buildMapFromPkg :: Maybe (PackageDecl SrcSpan) -> PiReader Expansion
buildMapFromPkg Nothing = return emptyExpansion
buildMapFromPkg (Just (PackageDecl pos n)) = 
    fmap snd $ buildMapFromImportName (TypeImportOnDemand pos n)



-- | Construct expansions for type declaration.
-- Returns (fully qualified name,
--          expansion for type itself,
--          expansion for all its super types)
buildMapFromTd :: Maybe (Name SrcSpan) -- ^ Name of surrounding package.
               -> TypeDecl SrcSpan     -- ^ The type declaration.
               -> Expansion            -- ^ Expansion of java.lang, imports etc.
               -> PiReader (Name SrcSpan, Expansion, Expansion)
buildMapFromTd mPkgPre td expn = do
  -- Extract ident, type params, super types from type declaration
  (pos, i, _tps, supers) <- 
      case td of
        ClassTypeDecl classDecl ->
          return ( ann classDecl
                 , cdId classDecl
                 , cdTypeParams classDecl
                 , maybe [] (:[]) (cdSuperClass classDecl)
                 )
        InterfaceTypeDecl intDecl ->
          return ( ann intDecl
                 , intdId intDecl
                 , intdTypeParams intDecl
                 , intdInterfaces intDecl
                 )
  -- Add package prefix to name
  let thisFullName = Name { nameAnn = pos
                          , nameId = i
                          , nameType = TypeName
                          , namePrefix = mPkgPre
                          }
                          
  -- Type expansion for the type declaration itself
  let tdExpn = mkTypeExpansionWithPrefix mPkgPre (idIdent i)
  
  -- Run name resolution on super types
  -- TODO ignoring type parameters for now.  
  rnSups <- runNameRes (mapM rnClassType supers) 
                       NameResEnv { nrCurrentName = thisFullName 
                                  , nrExpansion   = expn -- (expansionUnion [expn, (mkTpsExpn tps)])
                                  }
  superExpns <- mapM buildMapFromSuper rnSups
  
  return (thisFullName, tdExpn, expansionUnion   superExpns)

 where -- | Build expansion map for super type and its super types, recursively.
       buildMapFromSuper :: ClassType SrcSpan -> PiReader Expansion
       buildMapFromSuper ct = do
         mPre <- resolvePrefix (namePrefix $ ctName ct)
         let resName = (ctName ct) { nameType = TypeName, namePrefix = mPre }
         -- The super class/interface must of course also be a type
         withType resName $ do 
           cu             <- getTypeContents resName
           let sup = cuTypeDecls cu
           when (length sup /= 1) $
             failE $ unsupportedError "extending multiple types" defaultSpan
           (supSups, mDs) <- supersAndMembers (head sup)
           -- Recurse: Build map for super types farther up the hierarchy
           supExpns       <- mapM buildMapFromSuper supSups
           -- Expand the members - TODO ignoring locks for now...
           let resExpn = expandAll $ emptyExpansionRecord {
               -- nub for overloaded methods
               expandMethods = nub [ methodDeclId m | m@(MethodDecl {}) <- mDs ]
             , expandExps    = [ varDeclId v | f@(FieldDecl {}) <- mDs, 
                                               v@(VarDecl   {}) <- (fieldDeclVarDecls f) ]
             }
           -- Combine expansions of super-super types and super type itself
           return (expansionUnion $ resExpn : supExpns)

       -- | Return list of super types and list of members of the type.
       supersAndMembers :: TypeDecl SrcSpan 
                        -> PiReader ([ClassType SrcSpan], [MemberDecl SrcSpan])
       supersAndMembers superTd =
         case superTd of
           ClassTypeDecl classDecl -> return $
             ( maybe [] (:[]) (cdSuperClass classDecl)
             , [ memberDecl | MemberDecl memberDecl <- cbDecls $ cdBody classDecl ]
             )
           {-
           itD@(InterfaceTypeDecl {}) -> return $
             ( intdInterfaces $ tdIntDecl itD
             , ??  $ intdBody $ tdIntDecl itD
             )
           -}
           _ -> failEC ([],[]) $ unsupportedError "interface types" (ann superTd)
           
-- | Resolve the prefix of a name. Each part should be either a type or a
-- package name.
resolvePrefix :: Maybe (Name SrcSpan) -> PiReader (Maybe (Name SrcSpan))
resolvePrefix Nothing = return Nothing
resolvePrefix (Just name') = do
  mPre <- resolvePrefix (namePrefix name')
  let name = name' { namePrefix = mPre }
  debugPrint $ "Resolving prefix: " ++ unparsePrint name    
  case nameType name of
    PkgName       -> return (Just name)
    PkgOrTypeName -> do isType <- doesTypeExist name
                        isPkg  <- doesPkgExist  name
                        if isType
                         then do failEC () $ unsupportedError "inner types"
                                   (nameAnn name)
                                 return (Just $ name { nameType = TypeName })
                         else if isPkg
                           then return (Just $ name { nameType = PkgName })
                           else failEC (Just name) $
                             invalidPrefix (unparsePrint name) (nameAnn name)
    other         -> panic (thisModule ++ ".resolvePrefix") $
      "Unexpected name " ++ show name ++ " of nametype " ++ show other

