module Language.Java.Paragon.NameResolution.ExpansionRecord
  ( 
    -- * The @Expansion@ mapping
    ExpansionRecord(..)
    -- * Functionality
  , emptyExpansionRecord
  , expandAll
  ) where

import Language.Java.Paragon.Syntax
import Language.Java.Paragon.NameResolution.Expansion

-- | A record for collecting various types of idents that need to be expanded.
data ExpansionRecord = ExpansionRecord 
  { expandTypes   :: [Id]
  , expandMethods :: [Id]
  , expandLocks   :: [Id]
  , expandExps    :: [Id]
  }

-- | Empty expansion record.
emptyExpansionRecord :: ExpansionRecord
emptyExpansionRecord = ExpansionRecord
  { expandTypes   = []
  , expandMethods = []
  , expandLocks   = []
  , expandExps    = []
  }

-- | Perform expansion of all the idents in the expansion record.
expandAll :: ExpansionRecord -> Expansion
expandAll rec = expansionUnion $
  map (mkExpExpansion    . idIdent) (expandExps rec) ++
  map (mkLockExpansion   . idIdent) (expandLocks rec) ++
  map (mkMethodExpansion . idIdent) (expandMethods rec) ++
  map (mkTypeExpansion   . idIdent) (expandTypes rec)
