-- | Errors that may occur in the name resolution stage of the compiler.
module Language.Java.Paragon.NameResolution.Errors
  (
    -- * Errors
    invalidPrefix
  , unresolvedName
  , ambiguousName
  , illegalDeref
  , exprInPkgError
  ) where

import Data.Maybe (fromMaybe)

import Language.Java.Paragon.Error
import Language.Java.Paragon.Error.ErrorLabel
import Language.Java.Paragon.Interaction (unparsePrint)
import Language.Java.Paragon.SrcPos
import Language.Java.Paragon.Syntax

-- | Prefix (qualified part of @Name@) is neither a package nor a type.
invalidPrefix :: String -> MkError
invalidPrefix prefix =
  mkError $ defaultError 
    { pretty    = "Invalid prefix: " ++ prefix
    , explained = "Invalid prefix: " ++ prefix ++ " - could not find a package "
                  ++ "or type with this name."
    , labels    = [LBLError]
    }

-- | Name is not of expected type.
unresolvedName :: Name a -> MkError
unresolvedName n =
  mkError $ defaultError 
    { pretty    = "Could not resolve name " ++ (unparsePrint n) ++ 
                  " of presumed type " ++ (show $ nameType n)
    , labels    = [LBLError]
    }

-- | When taking the union of expansion maps, types of the same name do not
-- match.
ambiguousName :: String -> Maybe (Name SrcSpan) -> Maybe (Name SrcSpan) -> MkError
ambiguousName str t1 t2 =
  mkError $ defaultError 
    { pretty    = str ++ " could refer to both " ++                   
                  (fromMaybe "" (fmap unparsePrint t1)) ++ "." ++ str ++ " and "
                  ++ (fromMaybe "" (fmap unparsePrint t2)) ++ "." ++ str
    , labels    = [LBLError]
    }

-- | Trying to dereference something that can not be dereferenced.
illegalDeref :: String -> String -> MkError
illegalDeref ty name =
  mkError $ defaultError
    { pretty    = name ++ " is a " ++ ty ++ " and cannot be dereferenced."
    , labels    = [LBLError]
    }

-- | Expression appears with package-prefix.
exprInPkgError :: String -> Name SrcSpan -> Name SrcSpan -> MkError
exprInPkgError descr pkg name =
  mkError $ defaultError
    { pretty    = (unparsePrint name) ++ " is a " ++ descr ++ " and cannot " ++
                  "appear directly in package " ++ (unparsePrint pkg)
    , labels    = [LBLError]
    }
