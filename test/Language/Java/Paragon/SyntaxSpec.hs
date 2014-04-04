-- | Tests for automatic deriving of getAnn method from Annotated type class.
module Language.Java.Paragon.SyntaxSpec (main, spec) where

import Test.Hspec

import Language.Java.Paragon.Annotation
import Language.Java.Paragon.Syntax

-- | To be able to run this module from GHCi.
main :: IO ()
main = hspec spec

-- | Main specification function.
spec :: Spec
spec = do
  describe "ann" $ do
    it "returns annotation for leaf node" $
      getAnn (Null emptyAnnotation) `shouldBe` emptyAnnotation

    it "returns annotation for internal node (1 level)" $
      getAnn (Lit (Int emptyAnnotation 2)) `shouldBe` emptyAnnotation

    it "returns annotation for internal node (2 levels)" $ do
      let wrongAnnotation = emptyAnnotation { annIsNull = False }
      getAnn (RefType (ClassRefType (ClassType emptyAnnotation (Name wrongAnnotation (Id wrongAnnotation "Object") TypeName Nothing) [])))
        `shouldBe` emptyAnnotation
