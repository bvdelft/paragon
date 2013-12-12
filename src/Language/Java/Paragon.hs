module Main (main) where

import System.Environment (getArgs)

import Language.Java.Paragon.Error
import Language.Java.Paragon.Interaction.Flags
import Language.Java.Paragon.Parac

-- | Main method, invokes the compiler
main :: IO ()
main = do
  (flags, files) <- compilerOpts =<< getArgs
  mapM_ (compileFile flags) files

compileFile :: [Flag] -> String -> IO ()
compileFile flags file = do
  err <- parac flags file
  case err of
    [] -> return ()
    _  -> putStrLn $ showErrors err

showErrors :: [Error] -> String
showErrors []     = ""
showErrors (e:es) = showContext (errContext e)
                    ++ pretty e ++ "\n"
                    ++ showErrors es

showContext :: [ErrorContext] -> String
showContext []     = ""
showContext (c:cs) = context c ++ "\n"
                     ++ showContext cs
