module Main where

import Language.Java.Paragon.SourcePos
import Language.Java.Paragon.Error.Error
import Language.Java.Paragon.Error.ExampleErrors

-- | Main method, invokes the compiler
main :: IO ()
main = putStrLn $ explained (exampleErrorA "bla" defaultPos)
