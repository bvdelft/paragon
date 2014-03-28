-- | This module implements the name resolution stage of the compiler
module Language.Java.Paragon.NameResolution
  (
    -- * Resolve names
    resolveNames
  ) where

import Control.Monad (when)

import Language.Java.Paragon.Annotation
import Language.Java.Paragon.Error.StandardContexts
import Language.Java.Paragon.Error.StandardErrors
import Language.Java.Paragon.Monad.Base (failE, withErrCtxt)
import Language.Java.Paragon.Monad.NameRes
import Language.Java.Paragon.Monad.PiReader
import Language.Java.Paragon.SrcPos (defaultSpan)
import Language.Java.Paragon.Syntax

import Language.Java.Paragon.NameResolution.Expansion
import Language.Java.Paragon.NameResolution.ExpansionBuilders
import Language.Java.Paragon.NameResolution.Resolvers

-- | Here we resolve names in a single compilation unit (.para file) 
-- The method returns the same compilation unit with names resolved
-- Name resolution is based on the .pi files in the given pipath.
resolveNames :: CompilationUnit
             -> PiReader CompilationUnit
resolveNames cu = withErrCtxt (compPhaseContext "Name Resolution") $ do
  -- Check: Only supporting one type per compilation unit:                             
  when (length (cuTypeDecls cu) == 0) $
    failE $ unsupportedError "compilation unit without type definition" cu
  when (length (cuTypeDecls cu) /= 1) $
    failE $ unsupportedError "multiple types per compilation unit" cu
  -- 0. The package name 'java' is always in scope.
  let javaExpnMap = mkPkgExpansion "java"
  -- 1. Expand definitions from java.lang.
  (_, javaLangExpnMap) <- buildMapFromImportName $ TypeImportOnDemand
    (srcSpanToAnn defaultSpan) $ pkgName $ map (Id (srcSpanToAnn defaultSpan)) ["java", "lang"]
  -- 2. Expand definitions from imports.
  (imps, impExpnMap) <- buildMapFromImports (cuImportDecls cu)
  -- 3. Expand definitions from pi path.
  piExpnMap <- buildMapFromPiPath
  -- 4. Expand definitions from surrounding package.
  pkgExpnMap <- buildMapFromPkg (cuPkgDecl cu)
  -- _. Collect all the 'other' definitions.
  let jipExpnMap = expansionUnion [javaExpnMap, javaLangExpnMap, 
                                   impExpnMap, piExpnMap, pkgExpnMap]
  let td = head $ cuTypeDecls cu
  -- 5. Expand definition of and in the type itself & its super-types  
  (thisFullName, tnExpnMap, supExpnMap) <-
    buildMapFromTd (fmap pdName $ cuPkgDecl cu) td jipExpnMap
  -- 6. Construct final expansion context according to precedence rule
  -- (I.e. local definitions hide definitions in super types hide other defs)
  -- Using left-biased union
  let expnMap = expansionUnionLeftB [tnExpnMap, supExpnMap, jipExpnMap]
  -- 7. Now we can finally annotate the AST, resolving all names in the type
  -- declaration using the painstakingly constructed expansion map.
  td' <- runNameRes (rnTypeDecl td) $ NameResEnv { nrCurrentName = thisFullName 
                                                 , nrExpansion   = expnMap
                                                 }
  return $ cu { cuImportDecls = imps, cuTypeDecls = [td'] }
