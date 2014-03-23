-- | Defines the 'unparsing' type class, plus some general instances for
-- this type class. To avoid circular dependencies, all other instances should
-- be defined in the module defining the data type to be unparsed.
-- Unparsing is used in error messages and code generation.
module Language.Java.Paragon.Interaction.Unparse
  (
    -- * The @Unparse@ type class.
    Unparse(..)
  , unparsePrint
  ) where

import Text.PrettyPrint

-- | Class for unparsing.
class Unparse a where
  --  | Convert the object to unparse into a @Doc@. 
  unparse :: a -> Doc

instance Unparse a => Unparse [a] where
  unparse as = brackets $ hcat (punctuate (char ',') $ map unparse as)

unparsePrint :: Unparse a => a -> String
unparsePrint = show . unparse
