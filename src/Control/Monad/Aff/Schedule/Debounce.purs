module Control.Monad.Aff.Schedule.Debounce
  ( DebounceSettings(..)
  , debounce
  ) where

import Prelude

import Control.Monad.Aff (Aff, delay, forkAff)
import Control.Monad.Aff.MVar (AVAR, newEmptyMVar, takeMVar, putMVar)
import Control.Monad.Rec.Class (forever)
import Data.Time.Duration (Milliseconds)

import Control.Monad.Error.Class (try)


newtype DebounceSettings eff = DebounceSettings
  { frequency :: Milliseconds
  , action    :: Aff eff Unit
  }

debounce
  :: forall r
   . DebounceSettings (avar :: AVAR | r)
  -> Aff (avar :: AVAR | r) (Aff (avar :: AVAR | r) Unit)
debounce (DebounceSettings { frequency, action }) = do
  baton <- newEmptyMVar
  _ <- forkAff $ forever $ do
    _ <- takeMVar baton
    _ <- try action
    delay frequency
  pure $ void $ putMVar baton unit
