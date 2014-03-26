-- | Full compiler tests.
module Language.Java.Paragon.ParacSpec (main, spec) where

import Test.Hspec

import Control.Monad

import System.Directory
import System.FilePath

import Language.Java.Paragon.Error
import Language.Java.Paragon.Interaction.Flags
import Language.Java.Paragon.Parac

-- | To be able to run this module from GHCi.
main :: IO ()
main = hspec spec

testDir :: FilePath
testDir = "test" </> "paractests"

-- | Main specification function.
spec :: Spec
spec = do
  describe "Basic tests" $ do
    it "The first elementary test" $ do
      cf <- getCompFiles $ testDir </> "elementary"
      err <- mapM callParac cf
      -- With the current phases, there should be no errors. However with the
      -- policy constraint solving phase, there should be.
      mapM_ (\e -> e `shouldBe` []) err

-- | Given a filepath that contains a .compile file which instructs which files
-- should be compiled and in which order, relatively to that filepath. Runs the
-- paragon compiler on these files and returns the total list of errors.
callParac :: FilePath -> IO [Error]
callParac fp = do
  files <- fmap lines $ readFile (fp </> ".compile")
  res   <- mapM (\file-> parac [PiPath fp, SourcePath fp] file) files
  return $ concat res
  
-- | Returns all paths that contain .compile files found under the provided
-- path.
getCompFiles :: FilePath -> IO [FilePath]
getCompFiles fp = do
  cont <- getDirectoryContents fp
  let posDirs = map (fp </>) $ filter (\x -> head x /= '.') cont
  dirs <- filterM doesDirectoryExist posDirs     -- Filter possible directories
  rec  <- liftM concat $ mapM getCompFiles dirs  -- Search subfolders
  if ".compile" `elem` cont                      -- Possibly add this path
    then return $ fp : rec
    else return rec

{- Code for calling java compiler:
import System.Cmd (system)
import System.Exit (ExitCode(..))

  exits <- mapM (\f -> system $ "javac " ++ (paraToJava (fp </> f))) files
  -- Should we clean up class files here?
  mapM_ (shouldBe ExitSuccess) exits
  
-- | Changes file extension from .para to .java
paraToJava :: FilePath -> FilePath
paraToJava file =
  let (f,_ext) = splitExtension file
  in f <.> "java"
-}
