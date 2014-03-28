-- | Standard error contexts likely to be used often, for all general paragon
-- scoping occuring in multiple phases of the compilation.
module Language.Java.Paragon.Error.StandardContexts
  (
    -- * Error contexts
    compPhaseContext
  , classBodyContext
  , interfaceBodyContext
  , memberDeclContext
  ) where

import Data.List (intersperse)

import Language.Java.Paragon.Error
import Language.Java.Paragon.Syntax

-- Helper:

-- | Join an array (String) using a delimiter.
join :: [a] -> [[a]] -> [a]
join delim list = concat (intersperse delim list)

-- | In which phase of compilation.
compPhaseContext :: String -> ErrorContext
compPhaseContext phase =
  defaultContext { context = "During " ++ phase }

-- | While checking the body of the provided class.
classBodyContext :: ClassDecl -> ErrorContext
classBodyContext classDecl =
  let className = idIdent $ cdId classDecl
  in defaultContext { context = "In the body of class " ++ className }

-- | While checking the body of the provided interface.
interfaceBodyContext :: InterfaceDecl -> ErrorContext
interfaceBodyContext intDecl =
  let intdName = idIdent $ intdId intDecl
  in defaultContext { context = "In the body of interface " ++ intdName }

-- | While checking the declarations of member(s).
memberDeclContext :: MemberDecl -> ErrorContext
memberDeclContext fieldDecl@(FieldDecl {}) =
  let fieldNames = map (idIdent . varDeclId) (fieldDeclVarDecls fieldDecl)
  in defaultContext { context = "In the declaration of fields " ++ 
                                join ", " fieldNames }
memberDeclContext methodDecl@(MethodDecl {}) =
  let methodName = idIdent $ methodDeclId methodDecl
  in defaultContext { context = "In the declaration of method " ++ methodName }
