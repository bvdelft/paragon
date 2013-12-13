{-# LANGUAGE TemplateHaskell
 #-}

-- | This module provides a unified way of extracting annotations
-- from a syntax tree through the Annotated type class.
-- Template Haskell is used to automatically derive instances.
module Language.Java.Paragon.Annotated
  (
    Annotated(..)
  , deriveAnnotatedMany
  ) where

import Language.Haskell.TH

-- | Annotated type class.
-- Functor context is required to be able to easily transform annotations.
class Functor ast => Annotated ast where
  -- | Extracts the annotation field which should be the first one.
  ann :: ast l -> l

-- | Derives instances of Annotated type class for a given list of names (types).
deriveAnnotatedMany :: [Name] -> Q [Dec]
deriveAnnotatedMany = mapM deriveAnnotated

-- | Derives one instance of Annotated type class for a given name (type).
deriveAnnotated :: Name -> Q Dec
deriveAnnotated typeName = do
  TyConI decl <- reify typeName
  case decl of
    DataD dctx name typeVars constrs _ ->
      annInstance dctx name (map unTyVarBndr typeVars) constrs
    NewtypeD dctx name typeVars constr  _ ->
      annInstance dctx name (map unTyVarBndr typeVars) [constr]
    _ -> error "deriveAnnotated"

-- | Extracts the name of a type variable from a binder.
unTyVarBndr :: TyVarBndr -> Name
unTyVarBndr (PlainTV v) = v
unTyVarBndr (KindedTV v _) = v

-- | Produces an instance declaration.
annInstance :: Cxt     -- ^ Type declaration context.
            -> Name    -- ^ Type name.
            -> [Name]  -- ^ Type variables.
            -> [Con]   -- ^ Data constructors.
            -> Q Dec
annInstance dctx name typeVarNames constrs =
  instanceD context (conT ''Annotated `appT` typ)
    annDef
  where context = fmap (dctx ++) $ cxt $ map annPred (init typeVarNames)
        typ = foldl appT (conT name) $ map varT (init typeVarNames)
        annDef = [funD 'ann (map annFunClause constrs)]
        annPred n = classP ''Annotated [varT n]

-- | Produces a function clause for a given constructor.
annFunClause :: Con -> Q Clause
annFunClause (NormalC c ((_,VarT n):sts)) =
  clause [conP c (varP n : map (const wildP) sts)] (normalB (varE n)) []
annFunClause (RecC c sts) = annFunClause $ NormalC c [(s, t) | (_, s, t) <- sts]
annFunClause _ = error "annFunClause"

