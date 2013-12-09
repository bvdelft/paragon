-- | Defines various function in the context of user interaction,
-- most importantly the verbosity settings and logging mechanism.
module Language.Java.Paragon.Interaction.Panic
  (
    -- * Generic panic.
    panic
  ) where

import Language.Java.Paragon.Interaction.Headers (issueTracker)

-- | For potential bugs in the compiler. Stops execution and displays an error
-- message, with a request to report the bug to the issue tracker.
panic :: String  -- ^ Cause of the panic
      -> String  -- ^ Extra information
      -> a       -- ^ Execution is terminated, so can be used anywhere.
panic cause extra = error $ "Panic! " ++ cause ++ " caused the impossible,\
                            \and the world is now about to end in 3.. 2.. 1..\n\
                            \Please report as a bug at: " ++ issueTracker ++
                            if not (null extra)
                            then "\nExtra information: " ++ extra
                            else ""

