module Language.Java.Paragon.Parac
  (
    -- * The compiler
    parac
  ) where

import Data.Maybe (fromMaybe)

import Control.Exception (tryJust)
import Control.Monad (guard)
import System.Environment (getEnv)
import System.FilePath ((</>), splitSearchPath, splitFileName)
import System.IO.Error (isDoesNotExistError)

import Language.Java.Paragon.Error
import Language.Java.Paragon.Interaction hiding (pretty)
import Language.Java.Paragon.Monad.Base
import Language.Java.Paragon.Monad.PiReader
import Language.Java.Paragon.NameResolution
import Language.Java.Paragon.Parser
import Language.Java.Paragon.SrcPos

-- | Given the flags and a file to the compiler, run the compilation. 
parac :: [Flag] -> String -> IO [Error]
parac flags filePath = do
  piPath <- buildPiPath flags filePath
  erOrA <- runBaseM flags $ do
    detailPrint $ "Starting compilation of " ++ filePath
    debugPrint $ "With .pi-directories " ++ show piPath
    let sourcepath = fromMaybe "." (getSourcePath flags)
    content <- liftIO $ readFile $ sourcepath </> filePath
    case parse content filePath of
      Left e    -> failE (parseError e defaultSpan)
      Right ast -> do
        resolvedAST <- liftToBaseM piPath (resolveNames ast)
        detailPrint $ "Finished compilation of " ++ filePath ++ "\n"
        return resolvedAST
  case erOrA of
    Left  e  -> return e
    Right _  -> return []

-- | Collect paths to interface files from options and environment
buildPiPath :: [Flag] -> String -> IO [String]
buildPiPath flags filePath = do
  -- Using import System.FilePath, split path into directory and filename
  let (directoryRaw,_fileName) = splitFileName filePath
      -- Workaround for old and buggy 'filepath' versions
      -- Default is in this direcory
      _directory = if null directoryRaw then "./" else directoryRaw
  pp <- getPIPATH -- Read the PIPATH environment variable
  -- Concatenate two lists of directories where to look for pi files:
  -- ~ the explicitly defined ones with the -p flag
  -- ~ the ones coming from the environment variables
  -- If this set is empty, use the current directory.
  let pDirsSpec = concat [ splitSearchPath dir | PiPath dir <- flags ] ++ pp
      pDirs = if null pDirsSpec then ["./"] else pDirsSpec
  return pDirs

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
