-- | Module for handling selected IO operations.
module Language.Java.Paragon.Interaction.IO
  ( -- * Environmental variables
    getPIPATH
  ) where

import Control.Exception (tryJust)
import Control.Monad (guard)
import System.Environment (getEnv)
import System.FilePath (splitSearchPath)
import System.IO.Error (isDoesNotExistError)


getPIPATH :: IO [String]
getPIPATH = do
  -- guard indicates that the only expected exception is isDoesNotExistError
  -- returns an Either type, left exception, right path
  ePpStr <- tryJust (guard . isDoesNotExistError) $ getEnv "PIPATH"
  -- splitSearchPath comes from System.FilePath, splitting String into filepaths
  -- In case the PIPATH variable did not exist, the empty list is used.
  -- (either takes two functions, const makes a function ignoring the other
  -- argument, i.e. the exception is ignored).
  return $ splitSearchPath $ either (const []) id ePpStr
