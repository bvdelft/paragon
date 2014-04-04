-- | This module provides a unified way of extracting annotations
-- from a syntax tree through the Annotated type class.
module Language.Java.Paragon.Annotated
  ( -- * The @Annotated@ class
    Annotated(..)
  , modifyAnn
  ) where
  
import Language.Java.Paragon.Annotation

-- | Annotated type class.
class Annotated ast where
  -- | Extracts the annotation field.
  getAnn :: ast -> Annotation
  -- | Modify the annotation.
  setAnn :: Annotation -> ast -> ast

modifyAnn :: Annotated ast => (Annotation -> Annotation) -> ast -> ast
modifyAnn f x = setAnn (f $ getAnn x) x
