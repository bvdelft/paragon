module Main where

import System.Environment (getArgs)

import Language.Java.Paragon.SourcePos
import Language.Java.Paragon.Error.Error
import Language.Java.Paragon.Flags
import Language.Java.Paragon.Error.ExampleErrors

-- | Main method, invokes the compiler
main :: IO ()
main = do
  (_flags, _files) <- compilerOpts =<< getArgs
  putStrLn $ explained (exampleErrorA "bla" defaultPos)
  
