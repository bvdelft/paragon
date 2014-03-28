-- | Annotation data structure. Paragon uses the same annotation through-out the
-- entire compilation. Elements of the annotation are gradually filled by each
-- compilation phase.
module Language.Java.Paragon.Annotation
  (
    -- The @Annotation@ data type
    Annotation(..)
  , emptyAnnotation
  ) where

import Language.Java.Paragon.Interaction
import Language.Java.Paragon.SrcPos
import Language.Java.Paragon.TypeChecker.Types

-- | Annotation resulting from type checking.
data Annotation = Annotation
  { -- | Source span.
    annSrcSpan  :: SrcSpan
    -- | The type of the node if it has one, and whether this type is native for
    -- Paragon (i.e. is written in Java and compilation should not transform
    -- Paragon attributes for this type).
  , annType     :: Maybe (TcType, Bool)
    -- | Whether the value of this node can be null.
  , annIsNull   :: Bool
  } deriving (Show, Eq, Ord)

-- | Empty annotation. Any evaluation of an element of this annotation will
-- result in a panic.
emptyAnnotation :: Annotation
emptyAnnotation = Annotation
  { annSrcSpan = panic thisFunction $ msg "source span"
  , annType    = panic thisFunction $ msg "type"
  , annIsNull  = panic thisFunction $ msg "nullpointer"
  }
  where thisFunction = "Language.Java.Paragon.Annotation.defaultAnnotation"
        msg str = "The " ++ str ++ " annotation is not available in this phase."
