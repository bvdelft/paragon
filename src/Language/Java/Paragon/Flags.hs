-- | Module for handling the flags parsed to the compiler.
module Language.Java.Paragon.Flags
  (
    -- * The @Flag@ data type
    Flag(..)
    -- * Converting arguments to flags
  , compilerOpts
    -- * Querying the flags
  , getVerbosity
  , requestsVersion
  , requestsHelp
  , requestsNullCheck
  , getPiPath
  , getJavaOutputPath
  , getPiOutputPath
  , getSourcePath
  ) where

import Control.Monad (when)
import System.Console.GetOpt
import System.Exit (exitSuccess)

import Language.Java.Paragon.Headers

-- | All different flags that can be provided to the Paragon compiler
data Flag
  = -- | Verbosity level. Throughout the compiler, the various levels have the
    -- following verbosity:
    --
    --   0. No verbosity, the only result from the compiler is its return value.
    --
    --   1. Default verbosity. The compiler reports warnings and errors.
    --
    --   2. Reports for each file the phases that the compilation undergoes.
    --
    --   3. Reports mile-stone steps for each compilation phase. Default when
    --      argument is omitted.
    --
    --   4. Crazy.
    Verbose Int
  | -- | Current version number of the Paragon compiler, defined in "Paragon".
    Version
  | -- | Prints help information and quits
    Help
  | -- | Specify the path where the compiler looks for .pi files in addition 
    -- (and overruling?) those in the @PI_PATH@ environment variable.
    PiPath String
  | -- | Where to place generated .java files
    JavaOutputPath String
  | -- | Where to place generated .pi files
    PiOutputPath String
  | -- | Where to find the .para source files
    SourcePath String
  | -- | Disables checking for information flows via null pointer exceptions.
    NoNullCheck
  deriving (Show, Eq)


-- | Options for use with the @GetOpt@ library.
options :: [OptDescr Flag]
options =
  [ Option ['v'] ["verbose"]
      (OptArg (Verbose . maybe 3 read) "n") -- default verbosity is 3
      ("Control verbosity\n" ++
       "  n is 0--4\n" ++
       "  normal verbosity level is 1\n" ++
       "  -v alone is equivalent to -v3")
  , Option ['V'] ["version"]
      (NoArg Version)
      "Show version number"
  , Option ['h','?'] ["help"]
      (NoArg Help)
      "Show this help"
  , Option ['p'] ["pipath"]
      (ReqArg PiPath "<path>") -- required argument
      ( "Path to the root directory for Paragon\n" ++
        "interface (.pi) files (default is . )" )
  , Option [] ["javaOut"]
      (ReqArg JavaOutputPath "<path>")
      ( "Output directory for generated .java files\n" ++
        "(default is same as source)" )
  , Option [] ["piOut"]
      (ReqArg PiOutputPath "<path>")
      ( "Output directory for generated .pi files\n" ++
        "(default is same as source)" )
  , Option ['s'] ["source"]
      (ReqArg SourcePath "<path>")
      "Source directory for .para files (default is .)"
  , Option ['n'] ["nonull"]
      (NoArg NoNullCheck)
      ( "Do not check for flows via unchecked\n" ++
        "nullpointer exceptions" )
  ]

-- | Converts the input arguments to a set of flags and the set of files to be
-- compiled. All flags should appear before all files.
compilerOpts :: [String]              -- ^ Compiler arguments
             -> IO ([Flag], [String]) -- ^ Flags and files
compilerOpts argv =
  -- RequireOrder --> no option processing after first non-option 
  case getOpt RequireOrder options argv of
    (flags,files,[]) -> do
      when (Help    `elem` flags || null flags && null files) $ do
         putStrLn $ usageInfo usageHeader options
         exitSuccess
      return (flags,files)
    -- in case of errors: show errors parsing arguments + usage info
    (_,_,errs) -> ioError $ userError 
      (concat errs ++ usageInfo usageHeader options)
  

-- | Returns the verbosity if specified, defaults to 1.
getVerbosity :: [Flag] -> Int
getVerbosity flags = 
  case [ k | Verbose k <- flags ] of
    [k] -> k
    _   -> 1 -- Default

-- | Returns true if the @Version@ flag is present
requestsVersion :: [Flag] -> Bool
requestsVersion = elem Version

-- | Returns true if the @Help@ flag is present
requestsHelp :: [Flag] -> Bool
requestsHelp = elem Help

-- | Returns false if the @NoNullCheck@ flag is present
requestsNullCheck :: [Flag] -> Bool
requestsNullCheck = not . elem NoNullCheck

-- | Returns PiPath if specified, defaults to .
getPiPath :: [Flag] -> String
getPiPath flags =
  case [p | PiPath p <- flags] of
    [p] -> p
    _   -> "." -- Default.

-- | Returns .java output path, if specified.
getJavaOutputPath :: [Flag] -> Maybe String
getJavaOutputPath flags =
  case [p | JavaOutputPath p <- flags] of
    [p] -> Just p
    _   -> Nothing

-- | Returns .pi output path, if specified.
getPiOutputPath :: [Flag] -> Maybe String
getPiOutputPath flags =
  case [p | PiOutputPath p <- flags] of
    [p] -> Just p
    _   -> Nothing

-- | Returns path of .para files, if specified.
getSourcePath :: [Flag] -> Maybe String
getSourcePath flags =
  case [p | SourcePath p <- flags] of
    [p] -> Just p
    _   -> Nothing
