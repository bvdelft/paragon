module Main where

import Language.Java.Paragon.Error
import Language.Java.Paragon.Monad.Base

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Error accumulation" $ do
    it "gives no errors when no errors are thrown" $ do
      (Right res) <- runBaseM [] $ return "done"
      res `shouldBe` "done"
    it "gives no errors when all errors are caught" $ do
      (Right res) <- runBaseM [] $ do
         _ <- tryM (failE $ undefinedError "prr")
         return "done"
      res `shouldBe` "done"
