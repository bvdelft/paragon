-- | This module provides a representation of source code positions. The 
-- positions reported by the parsers should be converted to this format to
-- ensure that the rest of the code base does not need to be changed when the
-- choice of parser is changed.
module Language.Java.Paragon.SrcPos
  (
    -- * The @SrcPos@ data type
    SrcPos(..)
  , defaultPos
    -- * The @SrcSpan@ data type
  , SrcSpan(..)
  , mkSrcSpanFromPos
  , srcSpanToPos
  , combineSrcSpan
  ) where

-- | Paragon representation of source positions.
data SrcPos = SrcPos
  { srcFileName :: String  -- ^ Name of the source file.
  , srcLine     :: Int     -- ^ Line number (starting at 1).
  , srcColumn   :: Int     -- ^ Column number (starting at 1).
  } deriving (Show, Eq)

-- | Default source code position when none is available.
defaultPos :: SrcPos
defaultPos = SrcPos
  { srcFileName = "Source code position information not available"
  , srcLine     = -1
  , srcColumn   = -1
  }

-- | Source span. Delimits a portion of a source file.
-- All positions start at 1.
data SrcSpan = SrcSpan
  { srcSpanFileName    :: String -- ^ Name of the source file.
  , srcSpanStartLine   :: Int    -- ^ Line number of the beginning of the span.
  , srcSpanStartColumn :: Int    -- ^ Column number of the beginning of the span.
  , srcSpanEndLine     :: Int    -- ^ Line number of the end of the span.
  , srcSpanEndColumn   :: Int    -- ^ Column number of the end of the span.
  } deriving (Show, Eq)

-- | Construct source span from two source positions (start, end).
-- File name is taken from the start position.
mkSrcSpanFromPos :: SrcPos -> SrcPos -> SrcSpan
mkSrcSpanFromPos (SrcPos fileName startLine startColumn)
                 (SrcPos _        endLine   endColumn) =
  SrcSpan fileName startLine startColumn endLine endColumn

-- | Convert source span to source position by taking the start position.
srcSpanToPos :: SrcSpan -> SrcPos
srcSpanToPos (SrcSpan fileName l c _ _) = SrcPos fileName l c

-- | Combines two source spans into one by taking minimum of start positions
-- and maximum of end positions.
-- File name is taken from the first source span.
combineSrcSpan :: SrcSpan -> SrcSpan -> SrcSpan
combineSrcSpan (SrcSpan fileName startLine1 startColumn1 endLine1 endColumn1)
               (SrcSpan _        startLine2 startColumn2 endLine2 endColumn2) =
  SrcSpan fileName (min startLine1 startLine2) (min startColumn1 startColumn2)
                   (max endLine1   endLine2)   (max endColumn1   endColumn2)

