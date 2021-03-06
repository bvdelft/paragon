-- | Actual functionality of the PiReader.
module Language.Java.Paragon.Monad.PiReader.PiFunc
  (
    -- * Functionality of the @PiReader@.
    doesPkgExist
  , doesTypeExist
  , getPkgContents
  , getPiPathContents
  , getTypeContents
  ) where

import Control.Applicative ((<$>))

import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath ((</>))

import Language.Java.Paragon.Error.StandardErrors
import Language.Java.Paragon.Interaction
import Language.Java.Paragon.Monad.PiReader.Helpers
import Language.Java.Paragon.Monad.PiReader.MonadPR
import Language.Java.Paragon.Parser (parse)
import Language.Java.Paragon.Syntax (Name(..), CompilationUnit, NameType(..))

piReaderModule :: String
piReaderModule = libraryBase ++ ".Monad.PiReader.PiFunc"

-- | Checks if there is a directory corresponding to the given package name
-- in the pi-path environment.
doesPkgExist :: MonadPR m => Name -> m Bool
doesPkgExist pkgName = liftPR $ do
  let path = pkgNameToDir pkgName
  piPath <- getPiPath
  or <$> mapM (\p -> liftIO $ doesDirectoryExist $ p </> path) piPath

-- | Checks if there is a file corresponding to the given type name
-- in the pi-path environment. If a component of the prefix is a type
-- (determined by recursive calls) we return False since inner types are not
-- supported.
doesTypeExist :: MonadPR m => Name -> m Bool
doesTypeExist typeName = liftPR $ do
  tracePrint $ "Checking if type exists: " ++ unparsePrint typeName
  -- Might be a PkgName due to recursive calls.
  if not (nameType typeName `elem` [TypeName, PkgOrTypeName])
   then return False
   else -- Check for inner types:
    case namePrefix typeName of
      Just pre -> do 
        isType <- doesTypeExist pre
        if isType then failEC False $ unsupportedError "inner types" typeName
                  else cont
      Nothing  -> cont
    -- Not an inner type:
  where cont = do let path = typeNameToFile typeName
                  piPath <- getPiPath
                  tracePrint $ "Checking for type with piPath " ++ show piPath
                  go piPath path
                  where go [] _ = return False
                        go (p:pis) path = do
                              let fp = p </> path
                              debugPrint $ "Checking if type exists: " ++ fp
                              found <- liftIO $ doesFileExist fp
                              if found
                               then do
                                 debugPrint $ "Found " ++ fp
                                 return True
                               else go pis path

-- | Returns the list of all .pi files in the package, on the top-level.
-- Note: If more than 1 corresponding directory in path, the first is selected.
getPkgContents :: MonadPR m => Name -> m [String]
getPkgContents pkgName = liftPR $ do
  let path = pkgNameToDir pkgName
  piPath <- getPiPath
  completePath <- selectFirstPkg path piPath
  readContents completePath
  
      where selectFirstPkg :: FilePath -> [FilePath] -> PiReader FilePath
            selectFirstPkg _ [] = panic (piReaderModule ++ ".getPkgContents")
                                ("No such package exists - doesPkgExist not called successfully"
                                 ++ unparsePrint pkgName)
            selectFirstPkg path (pip:pips) = do
                     isP <- liftIO $ doesDirectoryExist $ pip </> path
                     if isP then return $ pip </> path
                            else selectFirstPkg path pips
                                 
            readContents :: FilePath -> PiReader [String]
            readContents path = do
              files <- liftIO $ getDirectoryContents path
              return $ filterPiIdents files

-- | Returns all the packages and types found at the top level of the pi-path.
-- The result is of the form (list of types, list of packages).
getPiPathContents :: MonadPR m => m ([String], [String])
getPiPathContents = do
  pp <- getPiPath
  liftIO $ go pp ([],[])

      where go :: [FilePath] -> ([String], [String]) 
               -> IO ([String], [String]) 
            go [] acc = return acc
            go (p:pis) (ts,ps) = do
                       isDir <- doesDirectoryExist p
                       if isDir then do
                                  files <- getDirectoryContents p
                                  pkgs  <- filterPkgIdentsM p files
                                  let tys = filterPiIdents files
                                  go pis (tys++ts,pkgs++ps)
                        else go pis (ts,ps)

-- | Find and parse .pi file for given AST name.
-- Note: If more than 1 corresponding file in path, the first is selected
getTypeContents :: MonadPR m => Name -> m CompilationUnit
getTypeContents n = liftPR $ do
  let path = typeNameToFile n
  piPath <- getPiPath
  findFirstPi path piPath

      where findFirstPi :: FilePath -> [FilePath] -> PiReader CompilationUnit
            findFirstPi _ [] = panic (piReaderModule ++ ".getTypeContents")
                               ("No such type exists - doesTypeExist not called successfully: "
                                ++ unparsePrint n)
            findFirstPi path (pip:pips) = do
                      isT <- liftIO $ doesFileExist $ pip </> path
                      if isT
                       then do fc <- liftIO $ readFile $ pip </> path
                               let pRes = parse fc $ pip </> path
                               case pRes of
                                 Right cu -> return cu
                                 Left pe  -> fail $ "Parse error in pi file for type " 
                                                   ++ unparsePrint n ++ ":\n"
                                                   ++ show pe
                       else findFirstPi path pips
