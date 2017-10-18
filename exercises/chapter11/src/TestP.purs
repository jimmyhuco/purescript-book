module TestP where


import Prelude

import Control.Monad.State (execState, modify)
import Data.Foldable (traverse_)
import Data.String (toCharArray)


testParens :: String -> Boolean
testParens "" = true
testParens str = result.open == result.close
  where
    result = execState (traverse_ checkParens $ toCharArray str) {open: 0, close: 0}
    checkParens '(' = modify (\s -> s {open = s.open + 1})
    checkParens ')' = modify (\s -> s {close = s.close + 1})
    checkParens _ = pure unit


