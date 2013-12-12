module Language.Java.Paragon.Parac
  (
    -- * The compiler
    parac
  ) where

import Data.Maybe (fromMaybe)
import System.FilePath ((</>))

import Language.Java.Paragon.Error
import Language.Java.Paragon.Interaction hiding (pretty)
import Language.Java.Paragon.Monad.Base
import Language.Java.Paragon.Parser

-- | Given the flags and a file to the compiler, run the compilation. 
parac :: [Flag] -> String -> IO String
parac flags file = do
  erOrA <- runBaseM flags $ do
    let sourcepath = fromMaybe "." (getSourcePath flags)
    content <- liftIO $ readFile $ sourcepath </> file
    let ast = parse content
    return (Right ast)
  case erOrA of
    Left  e  -> return $ showErrors e
    Right _  -> return ""

showErrors :: [Error] -> String
showErrors []     = ""
showErrors (e:es) = showContext (errContext e)
                    ++ pretty e ++ "\n"
                    ++ showErrors es

showContext :: [ErrorContext] -> String
showContext []     = ""
showContext (c:cs) = context c ++ "\n"
                     ++ showContext cs
