{-# LANGUAGE BangPatterns #-}

-- | Annotation data structure. Paragon uses the same annotation through-out the
-- entire compilation. Elements of the annotation are gradually filled by each
-- compilation phase.
module Language.Java.Paragon.Annotation
  (
    -- The @Annotation@ data type
    Annotation(..)
  , emptyAnnotation
  , srcSpanToAnn
  ) where

import Control.Exception (catch, SomeException)
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (catch)

import Language.Java.Paragon.Interaction.Panic
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
  } deriving (Show, Ord)

-- | Since every part of an annotation that has not been instantiated
-- yet (i.e. the phase that instantiates this part has not yet passed)
-- gives a @panic@, equality comparisons in early phases of the compiler
-- could give panics if we derived @Eq@ - in particular during testing.
-- Therefore we implement @Eq@ here, using @unsafePerformIO@ to consider
-- two attributes equal when they both give the same panic.
instance Eq Annotation where
  (==) annA annB =  panicSafeEq (annSrcSpan annA) (annSrcSpan annB)
                 && panicSafeEq (annType    annA) (annType    annB)
                 && panicSafeEq (annIsNull  annA) (annIsNull  annB)
   where panicSafeEq :: Eq a => a -> a -> Bool
         panicSafeEq x y = safeEval x == safeEval y
         -- | Guarantees to give a value. Either the intended value, but
         -- if it was an error message then this error message as
         -- string. Even better would be to have a special data type
         -- instead of a String, but safe here since none of the
         -- annotations is a String.
         safeEval :: Eq a => a -> Either String a
         safeEval val = unsafePerformIO $ 
                          catch ( do let !_forceEval = val
                                     return $ Right val ) 
                                (\err -> return $ Left $ show (err::SomeException))

           

-- | Empty annotation. Any evaluation of an element of this annotation will
-- result in a panic.
emptyAnnotation :: Annotation
emptyAnnotation = Annotation
  { annSrcSpan = panic thisFunction $ msg "source span"
  , annType    = panic thisFunction $ msg "type"
  , annIsNull  = panic thisFunction $ msg "nullpointer"
  }
  where thisFunction = "Language.Java.Paragon.Annotation.emptyAnnotation"
        msg str = "The " ++ str ++ " annotation is not available in this phase."

srcSpanToAnn :: SrcSpan -> Annotation
srcSpanToAnn s = emptyAnnotation { annSrcSpan = s }
