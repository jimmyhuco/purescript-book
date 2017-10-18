module Files where

import Prelude

import Control.Monad.Cont.Trans (ContT(..), runContT)
import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, Fn3, Fn4, runFn2, runFn3, runFn4)
import Types (Async)

foreign import data FS :: Effect

type ErrorCode = String

type FilePath = String

foreign import readFileImpl ::
                 forall eff. Fn3 FilePath
                   (String -> Eff (fs :: FS | eff) Unit)
                   (ErrorCode -> Eff (fs :: FS | eff) Unit)
                   (Eff (fs :: FS | eff) Unit)

foreign import writeFileImpl ::
                 forall eff. Fn4 FilePath
                   String
                   (Eff (fs :: FS | eff) Unit)
                   (ErrorCode -> Eff (fs :: FS | eff) Unit)
                   (Eff (fs :: FS | eff) Unit)

readFile :: forall eff. FilePath -> (Either ErrorCode String -> Eff (fs :: FS | eff) Unit) -> Eff (fs :: FS | eff) Unit
readFile path k = runFn3 readFileImpl path (k <<< Right) (k <<< Left)

writeFile :: forall eff. FilePath -> String -> (Either ErrorCode Unit -> Eff (fs :: FS | eff) Unit) -> Eff (fs :: FS | eff) Unit
writeFile path text k = runFn4 writeFileImpl path text (k $ Right unit) (k <<< Left)

readFileCont :: forall eff. FilePath -> Async (fs :: FS | eff) (Either ErrorCode String)
readFileCont path = ContT $ readFile path

writeFileCont :: forall eff. FilePath -> String -> Async (fs :: FS | eff) (Either ErrorCode Unit)
writeFileCont path text = ContT $ writeFile path text

readFileContEx :: forall eff. FilePath -> ExceptT ErrorCode (Async (fs :: FS | eff)) String
readFileContEx path = ExceptT $ readFileCont path

writeFileContEx :: forall eff. FilePath -> String -> ExceptT ErrorCode (Async (fs :: FS | eff)) Unit
writeFileContEx path text = ExceptT $ writeFileCont path text

copyFileContEx :: forall eff. FilePath -> FilePath -> ExceptT ErrorCode (Async (fs :: FS | eff)) Unit
copyFileContEx src dest = do
  content <- readFileContEx src
  -- @todo do语法提取的值直接是最内层？
  writeFileContEx dest content


copyFileCont :: forall eff . FilePath -> FilePath -> Async (fs :: FS | eff) (Either ErrorCode Unit)
copyFileCont src dest = do
  e <- readFileCont src
  case e of
    Left err -> pure $ Left err
    Right content -> writeFileCont dest content

test1 :: Eff (fs :: FS
             , console :: CONSOLE
             ) Unit
test1 = runContT
          (copyFileCont "/tmp/1.txt" "/tmp/2.txt")
          logShow

concatenate :: FilePath -> FilePath -> Eff (fs :: FS
             , console :: CONSOLE
             ) Unit
concatenate path1 path2 = runContT
  (do
    file1 <- readFileCont path1
    file2 <- readFileCont path2
    pure $ (<>) <$> file1 <*> file2)
  logShow


concatenate' :: FilePath -> FilePath -> Eff (fs :: FS
             , console :: CONSOLE
             ) Unit
concatenate' path1 path2 = runContT (runExceptT $
  (do
    file1 <- readFileContEx path1
    file2 <- readFileContEx path2
    pure $ file1 <> file2))
  logShow

type Milliseconds = Int

foreign import data TIMEOUT :: Effect

foreign import setTimeoutImpl
  :: forall eff. Fn2 Milliseconds
  (Eff (timeout :: TIMEOUT | eff) Unit)
  (Eff (timeout :: TIMEOUT | eff) Unit)

setTimeoutCont
  :: forall eff
  . Milliseconds
  -> Async (timeout :: TIMEOUT | eff) Unit
setTimeoutCont ms = ContT $ (\k -> runFn2 setTimeoutImpl ms (k unit))

testSetTimtout = runContT (setTimeoutCont 3000) (\k -> logShow "finished after 3 seconds")
