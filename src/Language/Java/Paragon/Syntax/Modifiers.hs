{-# LANGUAGE TemplateHaskell
           , DeriveFunctor
 #-}

-- | Paragon Abstract Syntax Tree. Modifiers.
module Language.Java.Paragon.Syntax.Modifiers
  (
    module Language.Java.Paragon.Syntax.Modifiers
  , module Language.Java.Paragon.Syntax.Expressions
  ) where

import Language.Java.Paragon.Syntax.Expressions

import Language.Java.Paragon.Annotated

-- | Modifiers for declarations.
data Modifier a
  = Public    a     -- ^ public
  | Protected a     -- ^ protected
  | Private   a     -- ^ private
  | Static    a     -- ^ static
  | Abstract  a     -- ^ abstract
  | Final     a     -- ^ final
  | Native    a     -- ^ native
  | Synchronized a  -- ^ synchronized
  | Transient a     -- ^ transient
  | Volatile  a     -- ^ volatile
  | StrictFP  a     -- ^ strictfp

  -- Paragon specific
  | Typemethod a  -- ^ typemethod
  | Reflexive  a  -- ^ reflexive
  | Transitive a  -- ^ transitive
  | Symmetric  a  -- ^ symmetric
  | Readonly   a  -- ^ readonly
  | Notnull    a  -- ^ notnull

  | Reads      a (Policy a)  -- ^ ?
  | Writes     a (Policy a)  -- ^ !

  -- TODO: more Paragon modifiers
  deriving (Show, Eq, Functor)

$(deriveAnnotatedMany [''Modifier])

