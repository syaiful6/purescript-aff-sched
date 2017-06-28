module Test.Control.Monad.Aff.MVarTest (main) where

import Prelude

import Control.Monad.Aff (Aff, forkAff, delay, cancel, attempt)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Aff.MVar (AVAR, newEmptyMVar, putMVar, takeMVar, killMVar)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Eff.Exception (error, throwException)
import Control.Monad.Rec.Class (forever)

import Data.Either (either)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.List ((..))


type TestMVar a = forall e. Aff (console :: CONSOLE, avar :: AVAR | e) a

timeout :: Milliseconds -> TestMVar Unit -> TestMVar Unit
timeout ms aff = do
  exn <- newEmptyMVar
  clr1 <- forkAff (delay ms *> putMVar exn (Just "Timed out"))
  clr2 <- forkAff (aff *> putMVar exn Nothing)
  res <- takeMVar exn
  log (show res)
  case res of
    Nothing -> void (clr1 `cancel` error "Done")
    Just e -> void (clr2 `cancel` error "Done") *> throwError (error e)

test_putTakeMVar :: TestMVar Unit
test_putTakeMVar = do
  v <- newEmptyMVar
  _ <- forkAff (delay (Milliseconds 0.0) *> putMVar v 1.0)
  a <- takeMVar v
  if a == 1.0
    then log "Success: put and take MVar"
    else throwError (error ("test_putTakeMVar fail. Expect 1.0, Got: " <> show a))

test_killMVar :: TestMVar Unit
test_killMVar = do
  v <- newEmptyMVar
  _ <- killMVar v (error "DOA")
  ev <- attempt $ takeMVar v
  either
    (const $ log "Success: Killed MVar")
    (const $ log "Failure: Oh noes, MVar survived!")
    ev

test_putMVar_block :: TestMVar Unit
test_putMVar_block = do
  mv <- newEmptyMVar
  _ <- forkAff $ traverse_ (putMVar mv >=> const (delay (Milliseconds 250.0))) (0..10)
  _ <- forkAff $ traverse_ (putMVar mv >=> const (delay (Milliseconds 250.0))) (11..20)
  void $ forkAff $ forever $ do
    v <- takeMVar mv
    log ("Received value " <> show v)

main :: TestMVar Unit
main = do
  log "Test put take MVar"
  test_putTakeMVar

  log "Test kill MVar"
  test_killMVar

  log "test_putMVar_block"
  test_putMVar_block
