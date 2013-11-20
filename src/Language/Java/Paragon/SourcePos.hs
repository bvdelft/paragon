-- | This module provides a representation of source code positions. The 
-- positions reported by the parsers should be converted to this format to
-- ensure that the rest of the code base does not need to be changed when the
-- choice of parser is changed.
module Language.Java.Paragon.SourcePos
  (
    -- * The @SourcePos@ data type
    SourcePos(..)
  , defaultPos
  ) where

-- | Paragon representation of source positions.
data SourcePos = SourcePos
  { fileName :: String  -- ^ Name of the source file.
  , line     :: Int     -- ^ Line number (starting at 1).
  , column   :: Int     -- ^ Column number (starting at 1).
  }

-- | Default source code position when none is available
defaultPos :: SourcePos
defaultPos = SourcePos
  { fileName = "Source code position information not available"
  , line     = -1
  , column   = -1
  }
