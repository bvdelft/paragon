{-# LANGUAGE DeriveDataTypeable #-}
module Language.Java.Paragon.PolicyLang.ActorSet
  ( -- * Actor sets
    ParagonActorSetRep
  ) where

import Data.Data

data ParagonActorSetRep = ASR
  deriving (Eq, Ord, Show, Data, Typeable)
