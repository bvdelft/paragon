module Language.Java.Paragon.TypeChecker
  ( -- * Type checking phase
    typeCheck
  ) where

import Language.Java.Paragon.Monad.Base
import Language.Java.Paragon.Monad.PiReader (PiPath)
import Language.Java.Paragon.Syntax

-- | Type checking phase. Returns an AST with type annotation for nodes that
-- have a type. The base name of the file containing the class of this AST is
-- required to check that the right type is defined in this .para file.
typeCheck :: PiPath     -- ^ Directories where .pi files can be found
          -> String     -- ^ Base name of the file
          -> AST        -- ^ AST from previous phase
          -> BaseM AST  -- ^ AST after type checking phase
typeCheck _piPath _baseName _ast = undefined
  
