module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Math (sqrt)
import Prelude (Unit, (*), (+))

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w * w + h * h)

main :: ∀ t4. Eff ( console ∷ CONSOLE | t4 ) Unit
main = logShow (diagonal 3.0 4.0)
