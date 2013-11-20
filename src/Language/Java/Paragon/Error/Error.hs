-- | Module defining the error record and the default instance.
module Language.Java.Paragon.Error.Error
  (
    -- * The @Error@ data type
    Error(..)
  , defaultError
  , mkError
  ) where

import Language.Java.Paragon.SourcePos
import Language.Java.Paragon.Error.ErrorLabel

-- | Error record structure. Each error should provide the following
-- information:
data Error = Error
  { -- | A pretty-printed version of this error, to be used in console output.
    pretty    :: String
    -- | A pretty-printed version of this error, to be used in console output.
    -- This message contains more detailed information explaining the 
    -- information-flow related causes of the error.
  , explained :: String
    -- | The source code position where this error originated.
  , location  :: SourcePos
    -- | The various labels an error might have. This /must/ always be a
    -- non-empty list, but might contain various labels. For example, an error
    -- can be both labeled as Error and as ImplicitFlow.
  , labels    :: [ErrorLabel]
  }

-- | Error containing default values for each field in the record. This error
-- should be used in all error-constructing functions to ensure that the 
-- addition of new record fields does not break existing errors.
defaultError :: Error
defaultError = Error
  { pretty    = "This error has no pretty printing"
  , explained = "This error has no explained printing"
  , location  = defaultPos
  , labels    = [LBLError]
  }

-- | adds the source code location to the error.
mkError :: Error -> SourcePos -> Error
mkError err sp = err { location = sp }
