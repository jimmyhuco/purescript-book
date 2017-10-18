module YargsDemo where
import Prelude

import Control.Monad.Eff.Console (log)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Node.Yargs.Applicative (Y, runY, yarg)
import Node.Yargs.Setup (usage)

merge :: String -> String -> String
merge x y = x <> y

main = runY (usage "$0 -p <player name>") $ map log env
  where
    env = merge <$> yarg "p" ["player"]
                             (Just "Player name")
                             (Right "The player name is required")
                             false
                <*> yarg "h" ["help"]
                             (Just "Test Help")
                             (Right "The help is required")
                             false
