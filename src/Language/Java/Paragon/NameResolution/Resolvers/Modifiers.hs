module Language.Java.Paragon.NameResolution.Resolvers.Modifiers
  (
    -- * Resolver
    rnModifier
  ) where

import Language.Java.Paragon.Monad.NameRes
import Language.Java.Paragon.Syntax.Modifiers

-- | Resolve modifiers - in particular, Paragon-specific modifiers.
rnModifier :: Resolve Modifier
rnModifier modifier = return modifier -- only for Paragon-modifiers
