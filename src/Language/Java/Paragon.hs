module Main where

import Language.Java.Paragon.Sub

-- | Main method, invokes the compiler
main :: IO ()
main = putStrLn "The end."

-- | Defines some data
sub :: MyData
sub = Something 4
