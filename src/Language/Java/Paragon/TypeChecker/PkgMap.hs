{-# LANGUAGE DeriveDataTypeable #-}
-- | This module provides the @PkgMap@ data structure.
module Language.Java.Paragon.TypeChecker.PkgMap
  (
    module Language.Java.Paragon.TypeChecker.PkgMap
  ) where

import Data.Data
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import Language.Java.Paragon.Syntax.Names
import Language.Java.Paragon.TypeChecker.TypeMap

data PkgMap = PkgMap
  { pmTypes        :: Map String TypeSignature
  , pmPackages     :: Map String PkgMap
  } deriving (Show, Data, Typeable)

-- | Empty package map. All mappings are empty.
emptyPkgMap :: PkgMap
emptyPkgMap = PkgMap
  { pmTypes    = Map.empty
  , pmPackages = Map.empty
  }

-- | Adds the new type signature to the package map.
insertPkgMapType :: Name -> TypeSignature -> PkgMap -> PkgMap
insertPkgMapType name typeSig pkgMap = 
  let nId = idIdent $ nameId name
  in updatePkgMapForPrefix (map idIdent $ flattenName name)
       (\pMap -> pMap { pmTypes = Map.insert nId typeSig (pmTypes pMap)})
       pkgMap
       
-- | Updates the package map that matches the provided prefix. If there is no
-- package map for a part of this prefix, the map is created.
updatePkgMapForPrefix :: [String]            -- ^ Prefix, eg @["java","lang"]@.
                      -> (PkgMap -> PkgMap)  -- ^ Update function.
                      -> PkgMap              -- ^ Map that is to be updated.
                      -> PkgMap              -- ^ Updated map.
updatePkgMapForPrefix [] updateFunc pkgMap = updateFunc pkgMap
updatePkgMapForPrefix (s:ss) updateFunc pkgMap = 
  let innerPkgMap   = fromMaybe emptyPkgMap (Map.lookup s (pmPackages pkgMap))
      updatedPkgMap = updatePkgMapForPrefix ss updateFunc innerPkgMap
  in pkgMap { pmPackages = Map.insert s updatedPkgMap (pmPackages pkgMap)}
