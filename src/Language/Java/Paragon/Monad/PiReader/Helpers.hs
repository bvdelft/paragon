-- | This module reads pi files.
module Language.Java.Paragon.Monad.PiReader.Helpers
  (
    -- * Helper functions for the @PiReader@.
    filterPiIdents
  , filterPkgIdentsM
  , packNameToDir
  , typeNameToFile
  ) where

import Control.Arrow (second)
import Control.Monad (filterM)

import Data.Char (toLower)

import System.FilePath ((</>),(<.>),splitExtension)
import System.Directory (doesDirectoryExist)

import Language.Java.Paragon.Interaction (panic, libraryBase)
import Language.Java.Paragon.Monad.Base (MonadIO(..))
import Language.Java.Paragon.Syntax (QId(..),NameType(..),Id(..))

prHelperModule :: String
prHelperModule = libraryBase ++ ".Monad.PiReader.Helpers"

-- | Return the names of all files with extension @.pi@. The extension itself
-- is dropped.
filterPiIdents :: [FilePath] -> [String]
filterPiIdents files =
    let fnses = map (second stringToLower . splitExtension) files
    in [ str | (str, ".pi") <- fnses, not (null str), head str /= '.' ]
  where stringToLower = map toLower

-- | Given a directory and a list of directory names relative to that directory,
-- returns the list of directories that actually exist. Hidden directories
-- (starting with @.@) are not included.
filterPkgIdentsM :: MonadIO m => FilePath -> [FilePath] -> m [String]
filterPkgIdentsM dir files = liftIO $ do
    fs <- filterM (doesDirectoryExist . (dir </>)) files    
    return [ f | f <- fs, head f /= '.' ]

-- | Convert an AST package name into a file path to the package directory.
packNameToDir :: QId a -> FilePath
packNameToDir packName =
  case qIdNameType packName of
    PkgName   ->
      case qIdPrevName packName of
        Just pre  ->  packNameToDir pre </> idName (qIdName packName)
        Nothing   ->  idName (qIdName packName)
    TypeName  ->  panic (prHelperModule ++ ".packNameToDir") 
                        "Inner types are not yet supported"
    _         ->  panic (prHelperModule ++ ".packNameToDir") (show packName)

-- | Convert AST type name into a file path to actual @.pi@ file.
typeNameToFile :: QId a -> FilePath
typeNameToFile typeName =
  case qIdNameType typeName of
    TypeName  ->
      case qIdPrevName typeName of
        Just pre  ->  packNameToDir pre </> idName (qIdName typeName) <.> "pi"
        Nothing   ->  idName (qIdName typeName) <.> "pi"
    _         ->  panic (prHelperModule ++ "typeNameToDir") (show typeName)
