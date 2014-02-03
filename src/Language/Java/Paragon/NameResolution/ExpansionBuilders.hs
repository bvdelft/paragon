-- | This module provides helper functionality for the name resolution stage in
-- compilation. The functions included here build expansion maps.
module Language.Java.Paragon.NameResolution.ExpansionBuilders
  (
    -- * Builders
    buildMapFromImportName
  ) where

import Language.Java.Paragon.Error.StandardErrors
import Language.Java.Paragon.Interaction
import Language.Java.Paragon.Monad.Base
import Language.Java.Paragon.Monad.PiReader
import Language.Java.Paragon.Syntax
import Language.Java.Paragon.SrcPos

import Language.Java.Paragon.NameResolution.Expansion

-- | Build expansion map from an import declaration
-- Returns declaration with resolved prefix and the constructed expansion.
-- Creates error when encountering an unsupported import, i.e. a static
-- import; the phase is continued with an empty expansion mapping for the
-- provided import.
-- TODO: check pattern matching on records.. seems odd?
buildMapFromImportName :: ImportDecl SrcSpan
                       -> PiReader (ImportDecl SrcSpan, Expansion)
buildMapFromImportName (SingleTypeImport _a imp) = do
  finePrint $ "Building expansion map for single import: " ++ show imp
  return undefined
  
buildMapFromImportName (TypeImportOnDemand _a imp) = do
  finePrint $ "Building expansion map for on-demand import: " ++ show imp
  return undefined
  
buildMapFromImportName other = do
  finePrint $ "Throwing error for static import: " ++ show other
  failEC (other, emptyExpansion) $ unsupportedError "static imports" (impdAnn other)
                                          
{-
    

    case imp of
      SingleTypeImport pos tn@(Name pos' TName mPre i) -> do
              -- Explicit import of a single type mPre.tn
              mPre' <- resolvePre mPre
              let resName = Name pos' TName mPre' i
                  resImp  = SingleTypeImport pos resName
                  resExpn = mkTExpansionWithPrefix mPre' (unIdent i)

              isTy <- doesTypeExist resName
              if isTy
               then return $ (resImp, resExpn)
               else failEC (resImp, Map.empty) . mkErrorFromInfo $ 
                 case mPre' of
                    Nothing -> UnresolvedName "type" (prettyPrint tn)
                    Just pre -> UnresolvedName "subpackage or type"
                                  (prettyPrint pre ++ "." ++ prettyPrint i)


      TypeImportOnDemand pos (Name pos' nt mPre i) 
          | nt `elem` [POrTName, PName] -> do
              -- We ignore the PorT ambiguity and resolve this import as if
              -- the name has to be a package, i.e. we're importing a whole
              -- package, not the inner types of a type
              -- This is, of course, only correct because we don't support
              -- inner types
              mPre' <- resolvePre mPre

              let resName = Name pos' PName mPre' i
                  resImp  = TypeImportOnDemand pos resName
                                          
              -- Resolve as package or fail
              withPackage resName
                (do 
                   piTypeIdents <- getPkgContents resName
                   let resExpn = expansionUnion $ 
                         map (mkTExpansionWithPrefix (Just resName)) piTypeIdents
                   return (resImp, resExpn))
      
      TypeImportOnDemand pos (Name pos' TName mPre i) -> do
              -- That name is a TName means this is an import statement for
              -- the inner classes of i
              mPre' <- resolvePre mPre
              let resName = Name pos' TName mPre' i
                  resImp  = TypeImportOnDemand pos resName

              withType resName
                (do -- resolve as type
                  _ast <- getTypeContents resName
                  -- TODO: Complete implementation: We don't actually support inner types yet
                  let resExpn = Map.empty
                  failEC (resImp, resExpn) $ mkError 
                    (UnsupportedFeature $ "Inner types not supported: " 
                                          ++ prettyPrint imp)
                    pos)
      
      _ -> do failEC (imp, Map.empty) $ mkErrorFromInfo
                (UnsupportedFeature $ "Static imports not yet supported: " 
                                          ++ prettyPrint imp)
-}
