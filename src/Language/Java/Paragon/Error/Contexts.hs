module Language.Java.Paragon.Error.Contexts
  (
    -- * Error contexts:
    exampleContextA
  ) where

import Language.Java.Paragon.Error

-- | Example context A, in practise the arguments to it should be relevant
-- types, likely not strings.
exampleContextA :: String -> MErrorContext
exampleContextA info =
  mkContext $ defaultContext 
    { context    = "In the context of " ++ info
    }
