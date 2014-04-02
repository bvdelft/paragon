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
  deriving (Data, Typeable, Show, Eq)

instance Annotated Modifier where
  ann (Public       x) = x
  ann (Protected    x) = x
  ann (Private      x) = x
  ann (Static       x) = x
  ann (Abstract     x) = x
  ann (Final        x) = x
  ann (Native       x) = x
  ann (Synchronized x) = x
  ann (Transient    x) = x
  ann (Volatile     x) = x
  ann (StrictFP     x) = x
  ann (Typemethod   x) = x
  ann (Reflexive    x) = x
  ann (Transitive   x) = x
  ann (Symmetric    x) = x
  ann (Readonly     x) = x
  ann (Notnull      x) = x
  ann (Reads      x _) = x
  ann (Writes     x _) = x
