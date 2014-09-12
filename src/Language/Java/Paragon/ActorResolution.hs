module Language.Java.Paragon.ActorResolution where

--import Language.Java.Paragon.Annotation
import Language.Java.Paragon.Error.StandardContexts
--import Language.Java.Paragon.Interaction
import Language.Java.Paragon.Monad.PiReader
import Language.Java.Paragon.Monad.Base
import Language.Java.Paragon.Syntax

thisModule :: String
thisModule = "Language.Java.Paragon.ActorResolution"

-- | Actor resolution phase. Accepts an AST with type information
-- for nodes that have a type. Returns an AST with best approximations
-- for actor identities, where applicable.
resolveActors :: AST           -- ^ AST from previous phase
              -> PiReader AST  -- ^ AST with actor identity information
resolveActors ast = withErrCtxt (compPhaseContext "Actor Resolution") $ do
  return ast