-- | Full compiler tests.
module Language.Java.Paragon.ParacSpec (main, spec) where

import Test.Hspec

import Control.Monad

-- Uncomment imports when enabling java compilation check
-- import System.Cmd (system)
import System.Directory
-- import System.Exit (ExitCode(..))
import System.FilePath

import Language.Java.Paragon.Interaction.Flags
import Language.Java.Paragon.Parac

-- | To be able to run this module from GHCi.
main :: IO ()
main = hspec spec

testDir :: FilePath
testDir = "test" </> "fulltests"

-- | Main specification function.
spec :: Spec
spec = do
  describe "Basic tests" $ do
    it "The first elementary test" $ do
      cf <- getCompFiles $ testDir </> "elementary"
      mapM_ testCompilation cf

-- | Given a filepath that contains a .compile file which instructs which files
-- should be compiled and in which order, relatively to that filepath. Runs the
-- paragon compiler on these files and expects an empty output. Runs the java
-- compiler on the resulting .java files and expects an @ExitSuccess@.
testCompilation :: FilePath -> IO ()
testCompilation fp = do
  files <- fmap lines $ readFile (fp </> ".compile")
  res   <- mapM (\file-> parac [PiPath fp, SourcePath fp] file) files
  -- Should we clean up generate java/pi files here?
  concat res `shouldBe` ""
  {- -- Disabled for now: java compilation
  exits <- mapM (\f -> system $ "javac " ++ (paraToJava (fp </> f))) files
  -- Should we clean up class files here?
  mapM_ (shouldBe ExitSuccess) exits
  -}

{-
-- | Changes file extension from .para to .java
paraToJava :: FilePath -> FilePath
paraToJava file =
  let (f,_ext) = splitExtension file
  in f <.> "java"
-}

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
