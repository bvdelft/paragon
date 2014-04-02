{-# LANGUAGE CPP, MultiParamTypeClasses, DeriveDataTypeable,
    FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
-- | This module provides a concrete instantiation of the Flow Locks Framework,
-- in the form of the Paragon dialect of the Paralocks language. The differences
-- from the presentation in Broberg & Sands, "Paralocks - Role-Based Information
-- Flow Control and Beyond", are a) the precision is improved for instance actor
-- fields, and b) the Paragon dialect can represent actor/lock type parameters.
module Language.Java.Paragon.PolicyLang 
  ( -- * Exported modules
    module Security.InfoFlow.Policy.FlowLocks
    -- * Paragon instances of the Flow Locks Framework
  , ParagonPolicy
  , ParagonVarPolicy
  , ParagonMetaPolicy
  ) where

import Security.InfoFlow.Policy.FlowLocks

import Language.Java.Paragon.Syntax (Name)

import Language.Java.Paragon.PolicyLang.ActorSet
import Language.Java.Paragon.PolicyLang.Policy

-- | Type synonym for the Paragon instance of @Policy@. This type of policy can
-- only be inhabited by concrete policies, as opposed to @ParagonVarPolicy@ and
-- @ParagonMetaPolicy@.
type ParagonPolicy =
  Policy
    Name
    ParagonActorSetRep  -- ^ Paragon representation of actor sets.

-- | Type synonym for the Paragon instance of @VarPolicy@. These policies may
-- contain concrete policies, policy variables (unknown but concrete, i.e. as
-- type parameters or using the @policyof@ construct), and combinations of these
-- using join and meet operations. These are the policies that can be created
-- by the Paragon programmer.
type ParagonVarPolicy =
  VarPolicy
    ParagonPolicyVarRep  -- ^ Paragon representation of regular variables.
    Name                 -- ^ Paragon representation of policy names.
    ParagonActorSetRep   -- ^ Paragon representation of actor sets.

-- | Type synonym for the Paragon instance of @MetaPolicy@. A @MetaPolicy@ can,
-- in addition to a @VarPolicy@ (see @PrgPolicy@), contain meta variables
-- representing the policies that need to be inferred (unification variables).
type ParagonMetaPolicy =
  MetaPolicy 
    ParagonMetaVarRep    -- ^ Paragon representation of meta variables.
    ParagonPolicyVarRep  -- ^ Paragon representation of regular variables.
    Name                 -- ^ Paragon representation of policy names.
    ParagonActorSetRep   -- ^ Paragon representation of actor sets.
