-- | Defines the pretty printing type class, plus some genearl instances for
-- this type class. To avoid circular dependencies, all other instances should
-- be defined in the module defining the data type to be pretty printed.
module Language.Java.Paragon.Interaction.Pretty
  (
    -- * The @Pretty@ type class.
    Pretty(..)
  , prettyPrint
  , module Text.PrettyPrint
  ) where

import Text.PrettyPrint

-- | Class for pretty printing.
class Pretty a where
  --  | Convert the object to pretty printed into a @Doc@. 
  pretty :: a -> Doc

instance Pretty a => Pretty [a] where
  pretty as = brackets $ hcat (punctuate (char ',') $ map pretty as)

prettyPrint :: Pretty a => a -> String
prettyPrint = show . pretty
