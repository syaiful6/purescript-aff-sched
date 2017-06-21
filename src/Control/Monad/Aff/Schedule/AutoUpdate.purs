module Control.Monad.Aff.Schedule.AutoUpdate
  ( UpdateSettings(..)
  , mkAutoUpdate
  , mkAutoUpdateWithModify
  ) where

import Prelude

import Control.Monad.Aff (Aff, delay, forkAff)
import Control.Monad.Aff.MVar (newEmptyMVar, takeMVar, putMVar, readMVar)
import Control.Monad.Eff.Ref (newRef, writeRef, readRef)
import Control.Monad.Eff.Class (liftEff)

import Data.Either (Either(..))
import Data.Time.Duration (Milliseconds)
import Data.Maybe (Maybe(..), maybe)

import Control.Monad.Aff.Schedule.Effects (ScheduleEff)


-- | Settings to control how values are updated. The first field specify the frequency
-- | and the second field is an Aff action to be performed to get the current value.
data UpdateSettings eff a = UpdateSettings Milliseconds (Aff eff a)

-- | Generate an action which will either read from an automatically updated value,
-- | or run the update action.
mkAutoUpdate
  :: forall r a
   . UpdateSettings (ScheduleEff r) a
  -> Aff (ScheduleEff r) (Aff (ScheduleEff r) a)
mkAutoUpdate us = mkAutoUpdateHelper us Nothing

-- | Generate an action which will either read from an automatically updated value,
-- | or run the update action if the first time or the provided modify action after that.
mkAutoUpdateWithModify
  :: forall r a
   . UpdateSettings (ScheduleEff r) a
  -> (a -> Aff (ScheduleEff r) a)
  -> Aff (ScheduleEff r) (Aff (ScheduleEff r) a)
mkAutoUpdateWithModify us f = mkAutoUpdateHelper us (Just f)

mkAutoUpdateHelper
  :: forall r a
   . UpdateSettings (ScheduleEff r) a
  -> Maybe (a -> Aff (ScheduleEff r) a)
  -> Aff (ScheduleEff r) (Aff (ScheduleEff r) a)
mkAutoUpdateHelper (UpdateSettings ms action) updateActionModify = do
  needsRunning <- newEmptyMVar
  responseVar0 <- newEmptyMVar
  currRef <- liftEff $ newRef $ Left responseVar0
  _ <- forkAff do
    let
      loop responseVar maybea = do
        _ <- takeMVar needsRunning
        a <- maybe action id (updateActionModify <*> maybea)
        _ <- liftEff $ writeRef currRef $ Right a
        _ <- putMVar responseVar a
        _ <- delay ms
        -- delay over
        responseVar' <- newEmptyMVar
        _ <- liftEff $ writeRef currRef $ Left responseVar'
        loop responseVar' (Just a)
    loop responseVar0 Nothing
  pure $ do
    mval <- liftEff $ readRef currRef
    case mval of
      Left responseVar -> do
        _ <- putMVar needsRunning unit
        readMVar responseVar
      Right val -> pure val
