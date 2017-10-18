module STDemo where

import Data.Either
import Data.Identity
import Prelude

import Control.Monad.Except.Trans (ExceptT(..), runExceptT, throwError)
import Control.Monad.State.Trans (StateT, get, lift, put, runStateT)
import Control.Monad.Writer.Trans (WriterT, runWriterT, tell)
import Data.Maybe (fromJust)
import Data.String (Pattern(..), drop, stripPrefix, take)
import Data.Tuple (Tuple)
import Partial.Unsafe (unsafePartial)

split :: StateT String (Either String) String
split = do
  s <- get
  case s of
    "" -> lift $ Left "Empty String"
    _ -> do
      put (drop 1 s)
      pure (take 1 s)

split_test :: Either String (Tuple String String)
split_test = runStateT (do
                           _ <- split
                           split) "test"

split_test2 :: Either String (Tuple String String)
split_test2 = runStateT ((<>) <$> split <*> split) "test"

type Errors = Array String
type Log = Array String
type Parser = StateT String (WriterT Log (ExceptT Errors Identity))

runIdentity :: forall a . Identity a -> a
runIdentity (Identity a) = a

runParser :: forall t85 t90 t91 t93. StateT t93 (WriterT t90 (ExceptT t85 Identity)) t91 -> t93 -> Either t85 (Tuple (Tuple t91 t93) t90)
runParser p s = runIdentity $ runExceptT $ runWriterT $ runStateT p s

split' :: Parser String
split' = do
  s <- get
  lift $ tell ["The state is " <> show s]
  case s of
    "" -> lift $ lift $ throwError ["Empty string"]
    _ -> do
      put (drop 1 s)
      pure (take 1 s)

split_test' = runParser (do
                            _ <- split'
                            split') "test"
split_test2' = runParser ((<>) <$> split' <*> split') "test"

safeDivide x 0 = throwError "zero"
safeDivide x y = pure $ x / y

string :: String -> Parser String
string str = do
  s <- get
  lift $ tell ["The state is " <> show s]
  case s of
    "" -> lift $ lift $ throwError ["Empty string"]
    _ -> do
      put (unsafePartial $ fromJust $ stripPrefix (Pattern str) s)
      pure str
