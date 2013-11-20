-- | Module defining various header constants used in the compiler.
module Language.Java.Paragon.Headers
  (
    -- * Headers
    usageHeader
  , paracVersionString
  , versionString
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
