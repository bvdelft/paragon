module Main where

import Control.Monad (when)
import System.Environment (getArgs)

import Language.Java.Paragon.SrcPos
import Language.Java.Paragon.Error
import Language.Java.Paragon.Flags
import Language.Java.Paragon.Interaction
import Language.Java.Paragon.Error.ExampleErrors
import Language.Java.Paragon.Error.Contexts
import Language.Java.Paragon.Monad.Base
import Language.Java.Paragon.Parser

-- | Main method, invokes the compiler
main :: IO ()
main = do
  (flags, _files) <- compilerOpts =<< getArgs
  let _ast = parse "package paragon.lang;" "Test.para"
  erOrA <- runBaseM flags $ do
    withErrCtxt (exampleContextA "clz" defaultPos) $ do
      i <- getFreshInt
      f <- getFlags
      when (getVerbosity f == 3) (liftIO $ putStrLn "Verbosity rulez")
      _ <- failE (exampleErrorB i defaultPos)
      when (getVerbosity f == 3) (debugPrint "Verbosity rulez2")
  case erOrA of
    Left  e  -> showErrors e
    Right _  -> putStrLn $ "jeeuj a"

showErrors :: [Error] -> IO ()
showErrors []     = return ()
showErrors (e:es) = do showContext (errContext e)
                       putStrLn (pretty e)
                       showErrors es

showContext :: [ErrorContext] -> IO ()
showContext []     = return ()
showContext (c:cs) = do putStrLn (context c)
                        showContext cs