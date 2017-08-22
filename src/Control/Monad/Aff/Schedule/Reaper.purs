{-
  This module provides the ability to create reapers: dedicated cleanup `threads`. These
  `threads` will automatically spawn and die based on the presence of a workload to process on.
  Example uses:
  * Killing long-running jobs
  * Closing unused connections in a connection pool
  * Pruning a cache of old items
-}

module Control.Monad.Aff.Schedule.Reaper
  ( ReaperSetting(..)
  , Reaper(..)
  , reaperAdd
  , reaperRead
  , reaperStop
  , reaperKill
  , mkReaper
  , mkListAction
  ) where

import Prelude

import Control.Monad.Aff (Aff, Canceler, cancel, forkAff, delay)
import Control.Monad.Aff.AVar (AVAR, AVar, makeVar', takeVar, putVar)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError, catchError)
import Control.Monad.Eff.Ref (Ref, newRef, writeRef, readRef, modifyRef')

import Data.Time.Duration (Milliseconds)
import Data.List (List(Nil), (:))
import Data.Maybe (Maybe(..))
import Partial (crashWith)
import Partial.Unsafe (unsafePartial)

import Control.Monad.Aff.Schedule.Effects (ScheduleEff)

-- | Settings for creating a reaper, This type has 3 parameters, ```eff``` is effect
-- | row for reaper action, ```workload``` gives the entire workload, and ```item```
-- | gives an individual piece of the queue. A common approach is to have ```workload```
-- | be a list of ```item```.
newtype ReaperSetting eff workload item = ReaperSetting
  { action :: workload -> Aff eff (workload -> workload)
  -- ^ The action to perform on a workload. The result of this is a workload modifying
  -- function. In the common case of using lists, the result should be a difference list
  -- that prepends the remaining workload to the temporary workload. See ```mkListAction```
  , delay  :: Milliseconds
  -- ^ delay between calls of 'reaperAction'
  , cons   :: item -> workload -> workload
  -- ^ Add an item onto a workload.
  , isNull :: workload -> Boolean
  -- ^ Check if a workload is empty, in which case the worker thread will shut down
  , empty  :: workload
  -- ^ An empty workload
  }

-- | A data structure to hold reaper APIS.
newtype Reaper eff workload item = Reaper
  { add  :: item -> Aff eff Unit
  , read :: Aff eff workload
  , stop :: Aff eff workload
  , kill :: Aff eff Unit
  }

-- | State of reaper.
data State workload = NoReaper | Workload workload

-- | Adding an item to the workload
reaperAdd :: forall eff workload item. Reaper eff workload item -> item -> Aff eff Unit
reaperAdd (Reaper { add }) = add

-- | Reading workload.
reaperRead :: forall eff workload item. Reaper eff workload item -> Aff eff workload
reaperRead (Reaper { read }) = read

-- | Stop the reaper thread if exists. The current workload is returned.
reaperStop :: forall eff workload item. Reaper eff workload item -> Aff eff workload
reaperStop (Reaper { stop }) = stop

-- | Killing the reaper immediately if exists.
reaperKill :: forall eff workload item. Reaper eff workload item -> Aff eff Unit
reaperKill (Reaper { kill }) = kill

-- | Create a reaper addition function. This function can be used to add
-- | new items to the workload. Spawning of reaper threads will be handled
-- | for you automatically.
mkReaper
  :: forall eff workload item
   . ReaperSetting (ScheduleEff eff) workload item
  -> Aff (ScheduleEff eff) (Reaper (ScheduleEff eff) workload item)
mkReaper settings@(ReaperSetting rset) = do
  stateRef <- liftEff $ newRef NoReaper
  tidRef   <- liftEff $ newRef Nothing
  lock     <- makeVar' unit
  pure $ Reaper
    { add: addItem lock settings stateRef tidRef
    , read: readState stateRef
    , stop: stop lock stateRef
    , kill: kill tidRef
    }
  where
  readState stateRef = do
    mx <- liftEff $ readRef stateRef
    case mx of
      NoReaper    -> pure rset.empty
      Workload wl -> pure wl
  stop lock stateRef = withAVar lock \_ -> do
    liftEff $ modifyRef' stateRef \mx -> case mx of
      NoReaper   -> { state: NoReaper, value: rset.empty }
      Workload x -> { state: Workload rset.empty, value: x }
  kill tidRef = do
    v <- liftEff $ readRef tidRef
    case v of
      Nothing -> pure unit
      Just x  -> cancel x (error "Fail to kill reaper") $> unit

addItem
  :: forall eff workload item
   . AVar Unit
  -> ReaperSetting (ScheduleEff eff) workload item
  -> Ref (State workload)
  -> Ref (Maybe (Canceler (ScheduleEff eff)))
  -> item
  -> Aff (ScheduleEff eff) Unit
addItem lock settings@(ReaperSetting rset) stateRef tidRef item = do
  next <- withAVar lock \_ -> liftEff $ modifyRef' stateRef cons
  next
  where
  cons NoReaper =
    let wl = rset.cons item rset.empty
    in { state: Workload wl, value: spawn lock settings stateRef tidRef }
  cons (Workload wl) =
    let wl' = rset.cons item wl
    in { state: Workload wl', value: pure unit }

spawn
  :: forall eff workload item
   . AVar Unit
  -> ReaperSetting (ScheduleEff eff) workload item
  -> Ref (State workload)
  -> Ref (Maybe (Canceler (ScheduleEff eff)))
  -> Aff (ScheduleEff eff) Unit
spawn lock settings stateRef tidRef = do
  canc <- forkAff $ reaper lock settings stateRef tidRef
  liftEff $ writeRef tidRef $ Just canc

reaper
  :: forall eff workload item
   . AVar Unit
  -> ReaperSetting (ScheduleEff eff) workload item
  -> Ref (State workload)
  -> Ref (Maybe (Canceler (ScheduleEff eff)))
  -> Aff (ScheduleEff eff) Unit
reaper lock settings@(ReaperSetting rec) stateRef tidRef = do
  _     <- delay rec.delay
  wl    <- withAVar lock \_ -> liftEff $ modifyRef' stateRef (\s -> unsafePartial $ swapWithEmpty s)
  merge <- rec.action wl
  next  <- withAVar lock \_ -> liftEff $ modifyRef' stateRef (\s -> unsafePartial $ check merge s)
  next
  where
  swapWithEmpty :: Partial => State workload -> { state :: State workload, value :: workload }
  swapWithEmpty NoReaper      = crashWith "Control.Monad.Aff.Schedule.Reaper: unexpected NoReaper (1)"
  swapWithEmpty (Workload wl) = { state: Workload rec.empty, value: wl }

  check :: Partial => (workload -> workload ) -> State workload -> { state :: State workload, value :: Aff (ScheduleEff eff) Unit }
  check _ NoReaper = crashWith "Control.Monad.Aff.Schedule.Reaper: unexpected NoReaper (2)"
  check merge (Workload wl) =
    let
      wl' = merge wl
    in
      if rec.isNull wl
        then { state: NoReaper, value: liftEff $ writeRef tidRef Nothing }
        else { state: Workload wl', value: reaper lock settings stateRef tidRef }

mkListAction
  :: forall eff item item'
   . (item -> Aff eff (Maybe item'))
  -> List item
  -> Aff eff (List item' -> List item')
mkListAction f = go id
  where
  go front Nil = pure front
  go front (x:xs) = do
    my <- f x
    let
      front' =
        case my of
          Nothing -> front
          Just y  -> front <<< (y : _)
    go front' xs

withAVar :: forall e a b. AVar a -> (a -> Aff (avar :: AVAR | e) b) -> Aff (avar :: AVAR | e) b
withAVar av k = do
  a <- takeVar av
  b <- k a `catchError` \e -> putVar av a *> throwError e
  putVar av a $> b