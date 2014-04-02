{-# LANGUAGE Rank2Types #-}
-- | Standard errors to be used only for very general problems (i.e. unsupported
-- features, unimplemented functionality, etc.)
module Language.Java.Paragon.Error.StandardErrors
  (
    -- * Errors
    unsupportedError
  ) where

import Language.Java.Paragon.Error
import Language.Java.Paragon.Error.ErrorLabel

-- | Unsupported features
unsupportedError :: String -> MkError
unsupportedError feature =
  mkError $ defaultError 
    { pretty    = "Unsupported feature: " ++ feature
    , explained = "The feature " ++ feature ++ " is currently not supported by"
                  ++ " the Paragon compiler."
    , labels    = [LBLError]
    }
