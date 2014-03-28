-- | This module provides a unified way of extracting annotations
-- from a syntax tree through the Annotated type class.
module Language.Java.Paragon.Annotated
  ( -- * The @Annotated@ class
    Annotated(..)
  ) where
  
import Language.Java.Paragon.Annotation

-- | Annotated type class.
class Annotated ast where
  -- | Extracts the annotation field which should be the first one.
  ann :: ast -> Annotation

