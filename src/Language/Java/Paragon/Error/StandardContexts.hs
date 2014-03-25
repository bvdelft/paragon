-- | Standard error contexts likely to be used often, for all general paragon
-- scoping occuring in multiple phases of the compilation.
module Language.Java.Paragon.Error.StandardContexts
  (
    -- * Error contexts
    classBodyContext
  ) where

import Language.Java.Paragon.Error
import Language.Java.Paragon.Syntax

-- | While checking the body of the provided class.
classBodyContext :: ClassDecl a -> ErrorContext
classBodyContext classDecl =
  let className = idIdent $ cdId classDecl
  in defaultContext { context = "In the body of class " ++ className }
