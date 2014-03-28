module Language.Java.Paragon.TypeChecker.Types 
  (
    TcType(..)
  ) where

data TcType = TcPrimT
  deriving (Show, Eq, Ord)
