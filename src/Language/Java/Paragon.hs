module Main (main) where

import System.Environment (getArgs)

import Language.Java.Paragon.Interaction.Flags
import Language.Java.Paragon.Parac

-- | Main method, invokes the compiler
main :: IO ()
main = do
  (flags, files) <- compilerOpts =<< getArgs
  mapM_ (\file -> parac flags file >>= putStrLn) files
