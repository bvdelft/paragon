-- | Tests for automatic deriving of ann method from Annotated type class.
module Language.Java.Paragon.SyntaxSpec (main, spec) where

import Test.Hspec

import Language.Java.Paragon.Syntax

-- | To be able to run this module from GHCi.
main :: IO ()
main = hspec spec

-- | Main specification function.
spec :: Spec
spec = do
  describe "ann" $ do
    it "returns annotation for leaf node" $
      ann (Null "ann") `shouldBe` "ann"

    it "returns annotation for internal node (1 level)" $
      ann (Lit (Int "ann" 2)) `shouldBe` "ann"

    it "returns annotation for internal node (2 levels)" $
      ann (RefType (ClassRefType (ClassType "ann" [(Id "annWrong" "Object", [])])))
        `shouldBe`
      "ann"

