-- | Example of how new errors should be created. Each phase that might generate
-- errors can define its own PhaseErrors.hs file that lists the errors
-- originating from that phase.
module Language.Java.Paragon.Error.ExampleErrors
  (
    -- * The errors defined in this module
    exampleErrorA
  , exampleErrorB
  ) where

import Language.Java.Paragon.SourcePos
import Language.Java.Paragon.Error
import Language.Java.Paragon.Error.ErrorLabel

-- | Example error A, in practise the arguments to the error should be relevant
-- types, likely not strings.
exampleErrorA :: String -> SourcePos -> Error
exampleErrorA info =
  mkError $ defaultError 
    { pretty    = "Something about " ++ info
    , explained = "This is an implicit flow"
    , labels    = [LBLError, LBLImplicit]
    }
    
-- | Example error B
exampleErrorB :: Int -> SourcePos -> Error
exampleErrorB i =
  mkError $ defaultError 
    { pretty    = "Something about " ++ (show i)
    , labels    = [LBLError]
    }
