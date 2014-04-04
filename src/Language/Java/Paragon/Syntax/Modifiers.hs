{-# LANGUAGE DeriveDataTypeable #-}
-- | Paragon Abstract Syntax Tree. Modifiers.
module Language.Java.Paragon.Syntax.Modifiers
  (
    module Language.Java.Paragon.Syntax.Modifiers
  , module Language.Java.Paragon.Syntax.Expressions
  ) where

import Data.Data

import Language.Java.Paragon.Syntax.Expressions

import Language.Java.Paragon.Annotation
import Language.Java.Paragon.Annotated

-- | Modifiers for declarations.
data Modifier
  = Public    Annotation     -- ^ public
  | Protected Annotation     -- ^ protected
  | Private   Annotation     -- ^ private
  | Static    Annotation     -- ^ static
  | Abstract  Annotation     -- ^ abstract
  | Final     Annotation     -- ^ final
  | Native    Annotation     -- ^ native
  | Synchronized Annotation  -- ^ synchronized
  | Transient Annotation     -- ^ transient
  | Volatile  Annotation     -- ^ volatile
  | StrictFP  Annotation     -- ^ strictfp

  -- Paragon specific
  | Typemethod Annotation  -- ^ typemethod
  | Reflexive  Annotation  -- ^ reflexive
  | Transitive Annotation  -- ^ transitive
  | Symmetric  Annotation  -- ^ symmetric
  | Readonly   Annotation  -- ^ readonly
  | Notnull    Annotation  -- ^ notnull

  | Reads      Annotation Policy  -- ^ ?
  | Writes     Annotation Policy  -- ^ !

  -- TODO: more Paragon modifiers
  deriving (Data, Typeable, Ord, Show, Eq)

instance Annotated Modifier where
  getAnn (Public       x) = x
  getAnn (Protected    x) = x
  getAnn (Private      x) = x
  getAnn (Static       x) = x
  getAnn (Abstract     x) = x
  getAnn (Final        x) = x
  getAnn (Native       x) = x
  getAnn (Synchronized x) = x
  getAnn (Transient    x) = x
  getAnn (Volatile     x) = x
  getAnn (StrictFP     x) = x
  getAnn (Typemethod   x) = x
  getAnn (Reflexive    x) = x
  getAnn (Transitive   x) = x
  getAnn (Symmetric    x) = x
  getAnn (Readonly     x) = x
  getAnn (Notnull      x) = x
  getAnn (Reads      x _) = x
  getAnn (Writes     x _) = x
  setAnn a (Public       _) = Public       a
  setAnn a (Protected    _) = Protected    a
  setAnn a (Private      _) = Private      a
  setAnn a (Static       _) = Static       a
  setAnn a (Abstract     _) = Abstract     a
  setAnn a (Final        _) = Final        a
  setAnn a (Native       _) = Native       a
  setAnn a (Synchronized _) = Synchronized a
  setAnn a (Transient    _) = Transient    a
  setAnn a (Volatile     _) = Volatile     a
  setAnn a (StrictFP     _) = StrictFP     a
  setAnn a (Typemethod   _) = Typemethod   a
  setAnn a (Reflexive    _) = Reflexive    a
  setAnn a (Transitive   _) = Transitive   a
  setAnn a (Symmetric    _) = Symmetric    a
  setAnn a (Readonly     _) = Readonly     a
  setAnn a (Notnull      _) = Notnull      a
  setAnn a (Reads      _ x) = Reads      a x
  setAnn a (Writes     _ x) = Writes     a x
