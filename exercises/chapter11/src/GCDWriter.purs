module GCDWriter where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.State (State, execState, modify)
import Control.Monad.Writer (Writer, execWriter, runWriter, tell)
import Data.Foldable (traverse_)
import Data.Monoid.Additive (Additive(..))

gcdLog :: Int -> Int -> Writer String Int
gcdLog n 0 = pure n
gcdLog 0 m = pure m
gcdLog n m = do
  _ <- tell $ "gcdLog " <> show n <> " " <> show m <> "; "
  if n > m
    then gcdLog (n - m) m
    else gcdLog n (m - n)

sumArray :: Array Int -> State Int Unit
sumArray = traverse_ \n -> modify \sum -> sum + n

sumArray' :: Array Int -> Writer (Additive Int) Unit
sumArray' = traverse_ \n -> tell $ Additive n

main :: ∀ t33. Eff ( console ∷ CONSOLE | t33 ) Unit
main = do
  log $ show $ runWriter (gcdLog 21 15)
  log $ show $
    execState (do
                  sumArray [1, 2, 3]
                  sumArray [4, 5]
                  sumArray [6]) 0
  log $ show $
    execWriter (do
                  sumArray' [1, 2, 3]
                  sumArray' [4, 5]
                  sumArray' [6])
