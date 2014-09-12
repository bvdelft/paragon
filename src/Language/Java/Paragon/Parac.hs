module Language.Java.Paragon.Parac
  (
    -- * The compiler
    parac
  ) where

import Data.Maybe (fromMaybe)

import System.FilePath ((</>), splitSearchPath, splitFileName)

import Language.Java.Paragon.Error
import Language.Java.Paragon.Interaction hiding (pretty)
import Language.Java.Paragon.Monad.Base
import Language.Java.Paragon.Monad.PiReader
import Language.Java.Paragon.NameResolution
import Language.Java.Paragon.Parser
import Language.Java.Paragon.TypeChecker (typeCheck)
import Language.Java.Paragon.ActorResolution (resolveActors)

-- | Given the flags and a file to the compiler, run the compilation. 
parac :: [Flag] -> String -> IO [Error]
parac flags filePath = do
  piPath <- buildPiPath flags filePath
  erOrU <- runBaseM flags $ do
    detailPrint $ "Starting compilation of " ++ filePath
    debugPrint $ "With .pi-directories " ++ show piPath
    let sourcepath = fromMaybe "." (getSourcePath flags)
    content <- liftIO $ readFile $ sourcepath </> filePath
    case parse content filePath of
      Left e    -> failE (parseError e errorAnnotation)
      Right ast -> runPiReader piPath $ do
        resolvedAST    <- raiseErrorsPR $ resolveNames ast
        typeCheckedAST <- raiseErrorsPR $ typeCheck filePath resolvedAST
        actorResAST    <- raiseErrorsPR $ resolveActors typeCheckedAST
        detailPrint $ "Finished compilation of " ++ filePath ++ "\n"
        debugPrint $ "AST after actor resolution:\n" ++ show actorResAST
        return ()
  case erOrU of
    Left  e  -> return e
    Right () -> return []

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
