{-# LANGUAGE DeriveDataTypeable #-}
module Language.Java.Paragon.PolicyLang.Policy
  ( -- * Policies
    ParagonMetaVarRep
  , ParagonPolicyVarRep
  ) where
  
import Data.Data

data ParagonMetaVarRep = PMVR
  deriving (Data, Typeable, Eq, Ord, Show)

data ParagonPolicyVarRep = PPVR
  deriving (Data, Typeable, Eq, Ord, Show)
