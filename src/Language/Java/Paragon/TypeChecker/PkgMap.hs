{-# LANGUAGE DeriveDataTypeable #-}
-- | This module provides the @PkgMap@ data structure.
module Language.Java.Paragon.TypeChecker.PkgMap
  (
    module Language.Java.Paragon.TypeChecker.PkgMap
  ) where

import Data.Data
import Data.Map (Map, empty)

import Language.Java.Paragon.TypeChecker.TypeMap

data PkgMap = PkgMap
  { pmTypes        :: Map String TypeSignature
  , pmPackages     :: Map String TypeMap
  } deriving (Show, Data, Typeable)

emptyPkgMap :: PkgMap
emptyPkgMap = PkgMap
  { pmTypes    = empty
  , pmPackages = empty
  }
