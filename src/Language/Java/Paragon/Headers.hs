-- | Module defining various header constants used in the compiler.
module Language.Java.Paragon.Headers
  (
    -- * Headers
    usageHeader
  , paracVersionString
  , versionString
  , issueTracker
  , libraryBase
  , typeCheckerBase
  ) where

-- | Version of the compiler.
versionString :: String
versionString = "0.3.0"

-- | Version of the compiler, including informing prefix.
paracVersionString :: String
paracVersionString = "Paragon Compiler version: " ++ versionString

-- | How to invoke the compiler.
usageHeader :: String
usageHeader = "Usage: parac [OPTION...] files..."

-- | URL pointing to the current issue tracker for paragon.
issueTracker :: String
issueTracker = "https://github.com/bvdelft/paragon/issues"

-- | Base path to the paragon module for error reporting.
libraryBase :: String
libraryBase = "Language.Java.Paragon"

-- | Base path to the paragon typecheck module for error reporting.
typeCheckerBase :: String
typeCheckerBase = libraryBase ++ ".TypeCheck"
