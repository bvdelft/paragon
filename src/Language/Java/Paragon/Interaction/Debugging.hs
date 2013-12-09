-- | Defines functions with various level of detail for printing debugging
-- information.
module Language.Java.Paragon.Interaction.Debugging
  (
    -- * Debug functions with increasing verbosity.
    normalPrint
  , detailPrint
  , finePrint
  , debugPrint
  , tracePrint
  ) where

import Control.Monad

import Language.Java.Paragon.Interaction.Flags
import Language.Java.Paragon.Monad.Base

-- | Only prints when verbosity is of requested level (Int) or above.
verbosePrint :: MonadBase m => Int -> String -> m ()
verbosePrint n str = do
  k <- liftM getVerbosity getFlags
  when (n <= k) $ liftIO $ putStrLn str

-- | Mapping print functions to call to the 'verbosePrint' function with
-- increasing verbosity

-- | Feedback to the user in the normal case (verbosity >= 1). Compiler reports
-- warnings and errors.
normalPrint :: MonadBase m => String -> m ()
normalPrint = verbosePrint 1

-- | Report for each file, each of the phases the compiler visits
-- (verbosity >= 2).
detailPrint :: MonadBase m => String -> m ()
detailPrint = verbosePrint 2

-- | Reports mile-stone steps for each compilation phase. Default when
-- argument is omitted (verbosity >= 3).
finePrint :: MonadBase m => String -> m ()
finePrint   = verbosePrint 3

-- | Include all debug output (verbosity >= 4).
debugPrint :: MonadBase m => String -> m ()
debugPrint  = verbosePrint 4

-- | Include all state and environment changes (verbosity >= 5).
tracePrint :: MonadBase m => String -> m ()
tracePrint  = verbosePrint 5


{-
This is a function that could be used to get potentially nicer formatting, but
includes dependency on haskell-src-exts. If included, import:

import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Pretty

formatData :: Show a => a -> String
formatData s = case parseExp (show s) of
    ParseOk x -> prettyPrintStyleMode
      -- ribbonsPerLine adjust how eager the printer is to break lines eg in records.
      Style{ mode = PageMode, lineLength = 150, ribbonsPerLine = 2.5 } defaultMode x
    ParseFailed{} -> show s
-}
