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
import Language.Java.Paragon.SrcPos

-- | Given the flags and a file to the compiler, run the compilation. 
parac :: [Flag] -> String -> IO [Error]
parac flags file = do
  erOrA <- runBaseM flags $ do
    let sourcepath = fromMaybe "." (getSourcePath flags)
    content <- liftIO $ readFile $ sourcepath </> file
    let ast = parse content
    failEC () (mkError defaultError defaultPos)
    return (Right ast)
  case erOrA of
    Left  e  -> return e
    Right _  -> return []
