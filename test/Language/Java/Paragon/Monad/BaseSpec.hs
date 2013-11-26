-- | Parser tests.
module Language.Java.Paragon.Monad.BaseSpec (main, spec) where

import Test.Hspec

import Language.Java.Paragon.Error
import Language.Java.Paragon.Monad.Base

-- | To be able to run this module from GHCi.
main :: IO ()
main = hspec spec

-- | Main specification function.
spec :: Spec
spec = do
  describe "Error accumulation" $ do
    it "gives no errors when no errors are thrown" $ do
      (Right res) <- runBaseM [] $ return "done"
      res `shouldBe` "done"
    it "gives no errors when all errors are caught" $ do
      (Right res) <- runBaseM [] $ do
         _ <- tryM (failE $ undefinedError "prr")
         return "done"
      res `shouldBe` "done"

