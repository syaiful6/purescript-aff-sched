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
import Control.Monad.Aff.AVar (AVAR, AVar, takeVar, putVar, makeVar')
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Error.Class (throwError, catchError)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Ref (Ref, newRef, writeRef, readRef, modifyRef')

import Data.Time.Duration (Milliseconds)
import Data.Maybe (Maybe(..))
import Data.List (List(Nil), (:))
import Partial (crashWith)
import Partial.Unsafe (unsafePartial)

import Control.Monad.Aff.Schedule.Effects (ScheduleEff)


newtype ReaperSetting eff workload item = ReaperSetting
  { action :: workload -> Aff eff (workload -> workload)
  , delay  :: Milliseconds
  , cons   :: item -> workload -> workload
  , isNull :: workload -> Boolean
  , empty  :: workload
  }

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
withAVar v aff = do
  a <- takeVar v
  b <- aff a `catchError` \e -> putVar v a *> throwError e
  putVar v a *> pure b
