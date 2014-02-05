-- | Helper functions for the Pi-reader.
module Language.Java.Paragon.Monad.PiReader.Helpers
  (
    -- * Helper functions for the @PiReader@.
    filterPiIdents
  , filterPkgIdentsM
  , pkgNameToDir
  , typeNameToFile
  ) where

import Control.Arrow (second)
import Control.Monad (filterM)

import Data.Char (toLower)

import System.FilePath ((</>),(<.>),splitExtension)
import System.Directory (doesDirectoryExist)

import Language.Java.Paragon.Interaction (panic, libraryBase, unparsePrint)
import Language.Java.Paragon.Monad.Base (MonadIO(..))
import Language.Java.Paragon.Syntax (Name(..), NameType(..), Id(..))

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
pkgNameToDir :: Name a -> FilePath
pkgNameToDir pkgName =
  case nameType pkgName of
    PkgName   ->
      case namePrefix pkgName of
        Just pre -> pkgNameToDir pre </> idIdent (nameId pkgName)
        Nothing  -> idIdent (nameId pkgName)
    TypeName  -> panic (prHelperModule ++ ".packNameToDir") 
                       "Inner types are not yet supported"
    _         -> panic (prHelperModule ++ ".packNameToDir") (unparsePrint pkgName)

-- | Convert AST type name into a file path to actual @.pi@ file.
typeNameToFile :: Name a -> FilePath
typeNameToFile typeName =
  case nameType typeName of
    TypeName  ->
      case namePrefix typeName of
        Just pre -> pkgNameToDir pre </> idIdent (nameId typeName) <.> "pi"
        Nothing  -> idIdent (nameId typeName) <.> "pi"
    _         -> panic (prHelperModule ++ "typeNameToDir") (unparsePrint typeName)

