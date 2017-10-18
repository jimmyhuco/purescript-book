module ParallelDemo where

import Control.Apply (lift2)
import Control.Monad.Cont (runContT)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Parallel (parallel, sequential)
import Files (FS, readFileCont)
import Prelude (Unit, append, flip, ($), (<$>), (<*>))

main :: forall t39.
  Eff
    ( console :: CONSOLE
    , fs :: FS
    | t39
    )
    Unit
main = flip runContT logShow do
  sequential $
    lift2 append
      <$> parallel (readFileCont "/tmp/1.txt")
      <*> parallel (readFileCont "/tmp/2.txt")
