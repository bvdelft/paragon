module Language.Java.Paragon.Error.ErrorLabel
  (
    -- * The @ErrorLabel@ data type
    ErrorLabel(..)
  ) where

-- | The various labels an error can have.
data ErrorLabel
  = LBLError     -- ^ Error (default label for errors).
  | LBLWarning   -- ^ Warnings
  | LBLExplicit  -- ^ Information leak via an explicit flow.
  | LBLImplicit  -- ^ Information leak via an implicit flow.
