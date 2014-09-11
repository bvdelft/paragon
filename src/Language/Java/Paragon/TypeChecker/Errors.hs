{-# LANGUAGE Rank2Types #-}

module Language.Java.Paragon.TypeChecker.Errors
  ( -- * Type checking errors
    fileNameMismatch
  ) where

import Language.Java.Paragon.Error
import Language.Java.Paragon.Syntax

-- | Class name does not match the file name.
fileNameMismatch :: String -> ClassDecl -> MkError
fileNameMismatch baseName classDecl =
  mkError $ defaultError 
    { pretty    = baseName ++ ".para should define class or interface " ++
                  baseName ++ ", found " ++ (idIdent $ cdId classDecl) ++
                  " instead."
    }

