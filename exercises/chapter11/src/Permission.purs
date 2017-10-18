module Permission where
import Data.String.Utils

import Control.Monad (bind, map, pure)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Reader (Reader, ask, local, runReader)
import Data.Maybe (Maybe(..))
import Data.Traversable (foldr, sequence)
import Prelude (Unit, flip, ($), (+), (-), (<>))

type Permissions = String
data User = User { name:: String,
                   comment:: String }

showUser :: Maybe User -> String
showUser (Just (User user)) = user.name <> " : " <> user.comment
showUser Nothing = "invalid user!"

hasPermission :: String -> Permissions -> Boolean
hasPermission = includes

addPermission :: String -> Permissions -> Permissions
addPermission str permit = case (hasPermission str permit) of
  true -> permit
  false -> permit <> " " <> str


createUser :: Reader Permissions (Maybe User)
createUser = do
  permissions <- ask
  if hasPermission "admin" permissions
    then map Just newUser
    else pure Nothing


newUser :: Reader Permissions User
newUser = pure $ User { name: "Jimmy",
                 comment: "He's studying the purescript language now." }

runAsAdmin :: forall a. Reader Permissions a -> Reader Permissions a
runAsAdmin = local (addPermission "admin")

createUserAsAdmin :: Reader Permissions (Maybe User)
createUserAsAdmin = runAsAdmin createUser

type Level = Int
type Doc = Reader Level String

line :: String -> Doc
line str = do
  currentLevel <- ask
  pure $ createDocStr currentLevel str
  where
    createDocStr 0 str = str <> "\n"
    createDocStr n str = "  " <> (createDocStr (n - 1) str)

indent :: Doc -> Doc
indent = local (\n -> n + 2)

cat :: Array Doc -> Doc
cat xs = do
  strs <- sequence xs
  pure $ foldr (\a b -> a <> b) "" strs

render :: Doc -> String
render = flip runReader 0

main :: Eff (console :: CONSOLE) Unit
main = do
  log $ render $ cat
    [ line "Here line 1"
    , indent $ cat
      [ line "Here line 2"
      , line "Here line 3"
      , indent $ line "Here line 4"
      ]
    ]
