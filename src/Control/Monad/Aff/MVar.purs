module Control.Monad.Aff.MVar
  ( MVar
  , newEmptyMVar
  , newMVar
  , takeMVar
  , tryTakeMVar
  , putMVar
  , tryPutMVar
  , readMVar
  , swapMVar
  , modifyMVar
  , modifyMVar'
  , withMVar
  , killMVar
  , module Control.Monad.Aff.AVar
  ) where

import Prelude

import Control.Monad.Aff (nonCanceler)
import Control.Monad.Aff.AVar (AVAR, AffAVar)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Error.Class (throwError, catchError)

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Function.Uncurried (Fn2, Fn3, Fn4, runFn2, runFn3, runFn4)

-- | MVar, unlike AVar this data type will block when put value.
foreign import data MVar :: Type -> Type

withMVar :: forall e a b. MVar a -> (a -> AffAVar e b) -> AffAVar e b
withMVar v aff = do
  a <- takeMVar v
  b <- aff a `catchError` \e -> putMVar v a *> throwError e
  putMVar v a *> pure b

newEmptyMVar :: forall eff a. AffAVar eff (MVar a)
newEmptyMVar = _makeMVar nonCanceler

newMVar :: forall eff a. a -> AffAVar eff (MVar a)
newMVar a = newEmptyMVar >>= \mv -> do
  _ <- runFn3 _putMVar nonCanceler mv a
  pure mv

takeMVar :: forall eff a. MVar a -> AffAVar eff a
takeMVar mv = runFn2 _takeMVar nonCanceler mv

tryTakeMVar :: forall eff a. MVar a -> AffAVar eff (Maybe a)
tryTakeMVar mv = runFn4 _tryTakeMVar nonCanceler Nothing Just mv

putMVar :: forall eff a. MVar a -> a -> AffAVar eff Unit
putMVar mv a = runFn3 _putMVar nonCanceler mv a

tryPutMVar :: forall e a. MVar a -> a -> AffAVar e Boolean
tryPutMVar mv a = runFn3 _tryPutMVar nonCanceler mv a

readMVar :: forall eff a. MVar a -> AffAVar eff a
readMVar mv = runFn2 _readMVar nonCanceler mv

-- | Take a value from an 'MVar', put a new value into the 'MVar' and
-- | return the value taken. This function is atomic only if there are
-- | no other producers for this 'MVar'.
swapMVar :: forall eff a. MVar a -> a -> AffAVar eff a
swapMVar mv new = do
  old <- takeMVar mv
  _ <- putMVar mv new
  pure old

killMVar :: forall e a. MVar a -> Error -> AffAVar e Unit
killMVar mv e = runFn3 _killMVar nonCanceler mv e

modifyMVar :: forall e a b. MVar a -> (a -> AffAVar e (Tuple a b)) -> AffAVar e b
modifyMVar v aff = do
  a <- takeMVar v
  Tuple a' b <- aff a `catchError` \e -> putMVar v a *> throwError e
  putMVar v a' *> pure b

modifyMVar' :: forall e a. MVar a -> (a -> AffAVar e a) -> AffAVar e Unit
modifyMVar' mv aff = do
  a <- takeMVar mv
  a' <- aff a `catchError` \e -> putMVar mv a *> throwError e
  putMVar mv a'

foreign import _makeMVar :: forall eff c a. c -> AffAVar eff (MVar a)

foreign import _takeMVar :: forall eff c a. Fn2 c (MVar a) (AffAVar eff a)

foreign import _tryTakeMVar :: forall eff c a. Fn4 c (forall x. Maybe x) (forall x. x -> Maybe x) (MVar a) (AffAVar eff (Maybe a))

foreign import _readMVar :: forall eff c a. Fn2 c (MVar a) (AffAVar eff a)

foreign import _putMVar :: forall eff c a. Fn3 c (MVar a) a (AffAVar eff Unit)

foreign import _tryPutMVar :: forall eff c a. Fn3 c (MVar a) a (AffAVar eff Boolean)

foreign import _killMVar :: forall eff c a. Fn3 c (MVar a) Error (AffAVar eff Unit)
