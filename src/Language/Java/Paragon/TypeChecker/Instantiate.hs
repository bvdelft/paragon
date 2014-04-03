-- | This module provides the functionality to substitute @TypeParam@ in an AST
-- node according to a provided substitution. This is used when instantiating
-- the type arguments of a class (both from .pi imports as well as the current
-- class under check), and when replacing the type parameters of the current
-- type declaration with skolem types.
module Language.Java.Paragon.TypeChecker.Instantiate
  ( -- * Type parameter substitution
    instantiate
  ) where

import Data.Data

import Language.Java.Paragon.Syntax
import Language.Java.Paragon.TypeChecker.Types

-- | Type parameters not yet supported.
instantiate :: Data a => [(TypeParam,TcTypeParam)] -> a -> a
instantiate _ = id
