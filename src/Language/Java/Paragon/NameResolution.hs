-- | This module implements the name resolution stage of the compiler
module Language.Java.Paragon.NameResolution
  (
    nameResModule
  , resolveNames
  ) where

import Language.Java.Paragon.Interaction.Headers (libraryBase)
import Language.Java.Paragon.SrcPos (SrcSpan, defaultSpan)
import Language.Java.Paragon.Syntax
import Language.Java.Paragon.Monad.PiReader

import Language.Java.Paragon.NameResolution.ExpansionBuilders

-- | Base name for error messages from this module
nameResModule :: String
nameResModule = libraryBase ++ ".NameResolution"


-- | Here we resolve names in a single compilation unit (.para file) 
-- The method returns the same compilation unit with names resolved
-- Name resolution is based on the .pi files in the given pipath.
resolveNames :: CompilationUnit SrcSpan
             -> PiReader (CompilationUnit SrcSpan)
resolveNames ast = do --
{- (CompilationUnit pos pkg imps [td]) 
pos = cuAnn ast
pkg = cuPkgDecl ast
imps = cuImportDecls ast
[td] = cuTypeDecls ast
-} 
  -- 1. Expand definitions from java.lang
  (_, _javaLangExpnMap) <- buildMapFromImportName $
       TypeImportOnDemand defaultSpan (pkgOrTypeName [Id defaultSpan "java", Id defaultSpan "lang"])
  return ast
  {-
  -- 2. Expand definitions from imports
  (imps', impExpnMap) <- buildMapFromImports imps
  
  -- 3. Expand definitions from pi path
  piExpnMap           <- buildMapFromPiPath
  
  -- 4. Expand definitions from surrounding package
  pkgExpnMap          <- buildMapFromPkg pkg
  
  -- 5. Expand definition of and in the type itself & its super-types
  let jipExpnMap = unionExpnMaps [javaExpnMap, javaLangExpnMap, 
                                  impExpnMap, piExpnMap, pkgExpnMap]
  (thisFullName,tnExpnMap,supExpnMap) <- buildMapFromTd (unPkgDecl pkg) td jipExpnMap
  
  -- 6. Construct final expansion context according to precedence rule
  -- (I.e. local definitions hide definitions in super types hide other defs)
  let expnMap = Map.union tnExpnMap (unionExpnMaps [jipExpnMap,supExpnMap])
  
  -- 7. Now we can finally annotate the AST, resolving all names in the type
  -- declaration using the painstakingly constructed expansion map
  --debugPrint $ "Expansions: "
  --mapM_ (debugPrint . show) $ Map.toList expnMap
  td' <- runNameRes (rnTypeDecl td) thisFullName expnMap
  return $ CompilationUnit pos pkg imps' [td']
  -}
