{-# LANGUAGE Rank2Types #-}
-- | Module defining the error record and the default instance.
module Language.Java.Paragon.Error
  (
    -- * The @Error@ data type
    Error(..)
  , ContextualError
  , defaultError
  , mkError
  , MkError
  , undefinedError
   -- * The @ErrorContext@ data type
  , ErrorContext(..)
  , defaultContext
  , errorAnnotation
  ) where

import Language.Java.Paragon.Annotated
import Language.Java.Paragon.Annotation
import Language.Java.Paragon.SrcPos
import Language.Java.Paragon.Error.ErrorLabel

-- | Error record structure. Each error should provide the following
-- information:
data Error = Error
  { -- | A pretty-printed version of this error, to be used in console output.
    pretty     :: String
    -- | A pretty-printed version of this error, to be used in console output.
    -- This message contains more detailed information explaining the 
    -- information-flow related causes of the error.
  , explained  :: String
    -- | The source code span where this error originated.
  , location   :: SrcSpan
    -- | Context in which the error occurred.
  , errContext :: [ErrorContext]
    -- | The various labels an error might have. This /must/ always be a
    -- non-empty list, but might contain various labels. For example, an error
    -- can be both labeled as Error and as ImplicitFlow.
  , labels     :: [ErrorLabel]
  }
  deriving (Eq, Show)

-- | Error that is needs to be put in context
type ContextualError = [ErrorContext] -> Error

-- | Error containing default values for each field in the record. This error
-- should be used in all error-constructing functions to ensure that the 
-- addition of new record fields does not break existing errors.
defaultError :: Error
defaultError = Error
  { pretty     = "This error has no pretty printing"
  , explained  = "This error has no explained printing"
  , location   = defaultSpan
  , errContext = [defaultContext]
  , labels     = [LBLError]
  }

data ErrorAnnotation = ErrorAnnotation Annotation

instance Annotated ErrorAnnotation where
  getAnn (ErrorAnnotation x) = x
  setAnn a _ = ErrorAnnotation a

-- | When an annotation is not available to provide to an error, this
-- annotation can be used. It gives the default span as source location.
errorAnnotation :: ErrorAnnotation
errorAnnotation = ErrorAnnotation $
  emptyAnnotation { annSrcSpan = defaultSpan }

-- | Adds the source code location and error context to the error.
mkError :: Annotated a => Error -> a -> [ErrorContext] -> Error
mkError err el ctx = err { location   = annSrcSpan $ getAnn el
                         , errContext = ctx
                         }

-- | Type abbreviation to simplify error-defining source code. We
-- require an annotated object to identify the source location (and not
-- the source location itself to simplify the call to an error; the
-- source location is extracted from the error in the @mkError@
-- function). When no annotated object can be used for this purpose,
-- @errorAnnotation@ can be used.
type MkError = Annotated a => a -> ContextualError

-- | For lazy or for handling errors via 'fail'
undefinedError :: String -> [ErrorContext] -> Error
undefinedError err ec =
  defaultError 
    { pretty     = "undefinedError: " ++ err
    , explained  = "undefinedError: " ++ err
    , errContext = ec
    }

-- | Context in which errors can occur (class, method, constructor...).
data ErrorContext = ErrorContext
  { -- | A pretty-printed version of this context, used in console output.
    context      :: String
  }
  deriving (Eq, Show)

-- | Default context; any context should be extending this context for the sake
-- of modularity.
defaultContext :: ErrorContext
defaultContext = ErrorContext
  { context      = "This error context has no pretty printing"
  }
