module Control.Monad.Future.Internal where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Apply (lift2)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Eff.Ref (newRef, readRef, writeRef)
import Control.Monad.Eff.Ref.Unsafe (unsafeRunRef)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.Monad.Rec.Class (class MonadRec, Step(..))
import Control.MonadPlus (class MonadPlus)
import Control.MonadZero (class MonadZero)
import Control.Parallel (parSequence_)
import Control.Parallel.Class (class Parallel)
import Control.Plus (class Plus, empty)

import Data.Either (Either(..), isLeft)
import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype)
import Data.Time.Duration (Milliseconds(..))

import Partial.Unsafe (unsafeCrashWith)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Future :: # Effect -> Type -> Type

foreign import data ASYNC :: Effect

newtype Thread eff a = Thread
  { kill :: Error -> Future eff Unit
  , join :: Future eff a
  }

newtype ParFuture eff a = ParFuture (Future eff a)

newtype Canceler eff = Canceler (Error -> Future eff Unit)

nonCanceler :: forall eff. Canceler eff
nonCanceler = Canceler k
  where
    k _ = pure unit

delay :: forall eff. Milliseconds -> Future eff Unit
delay (Milliseconds n) = Fn.runFn2 _delay Right n

launchFuture :: forall eff a. Future eff a -> Eff (async :: ASYNC | eff) (Thread eff a)
launchFuture ft = Fn.runFn6 _launchFuture isLeft unsafeFromLeft unsafeFromRight Left Right ft

unsafeLaunchFuture :: forall eff a. Aff eff a -> Eff eff (Thread eff a)
unsafeLaunchFuture = unsafeCoerce launchAff

instance functorFuture :: Functor (Future eff) where
  map f aff = Fn.runFn2 _map f aff

instance applyFuture :: Apply (Future eff) where
  apply = ap

instance applicativeFuture :: Applicative (Future eff) where
  pure = _pure

instance bindFuture :: Bind (Future eff) where
  bind ta k = Fn.runFn2 _bind ta k

instance monadFuture :: Monad (Future eff)

instance semigroupFuture :: Semigroup a => Semigroup (Future eff a) where
  append = lift2 append

instance monoidFuture :: Monoid a => Monoid (Future eff a) where
  mempty = pure mempty

instance altFuture :: Alt (Future eff) where
  alt a1 a2 = do
    res <- attempt a1
    case res of
      Left err -> a2
      Right r  -> pure r

instance plusTask :: Plus (Future eff) where
  empty = throwError (error "Always fails")

instance alternativeFuture :: Alternative (Future eff)

instance monadZeroFuture :: MonadZero (Future eff)

instance monadPlusFuture :: MonadPlus (Future eff)

instance monadRecFuture :: MonadRec (Future eff) where
  tailRecM k = go
    where
      go a = k a >= case _ of
        Done r -> pure r
        Loop b -> go b

instance monadThrowFuture :: MonadThrow Error (Future eff) where
  throwError = _throwError

instance monadErrorFuture :: MonadError Error (Future eff) where
  catchError ft k = do
    res <- attempt ft
    case res of
      Left err -> k err
      Right r  -> pure r

instance monadEffFuture :: MonadEff eff (Future eff) where
  liftEff = _liftEff

derive instance newtypeParFuture :: Newtype (ParFuture eff a) _
derive newtype instance functorParFuture :: Functor (ParFuture eff)

instance applyParFuture :: Apply (ParFuture eff) where
  apply (ParFuture ff) (ParFuture fa) = ParFuture (makeFuture go)
    where
      go k = do
        Thread t1 <- unsafeLaunchFuture ff
        Thread t2 <- unsafeLaunchFuture fa
        Thread t3 <- unsafeLaunchFuture do
          f <- attempt t1.join
          a <- attempt t2.join
          liftEff (k (f <*> a))
        pure $ Canceler \err ->
          parSequence_
            [ t3.kill err
            , t1.kill err
            , t2.kill err
            ]

instance applicativeParFuture :: Applicative (ParFuture eff) where
  pure = ParAff <<< pure

instance semigroupParFuture :: Semigroup a => Semigroup (ParFuture eff) where
  append = lift2 append

instance monoidParFuture :: Monoid a => Monoid (ParFuture eff a) where
  empty = pure mempty

instance altParFuture :: Alt (ParFuture eff) where
  alt (ParAff a1) (ParAff a2) = ParAff (makeFuture go)
    where
      go k = do
        ref <- unsafeRunRef $ newRef Nothing
        Thread t1 <- unsafeLaunchFuture a1
        Thread t2 <- unsafeLaunchFuture a2
        let
          earlyError = error "Alt ParAff: early exit"
          runK t r = do
            res <- liftEff $ unsafeRunRef $ readRef ref
            case res, r of
              Nothing, Left _  -> liftEff $ unsafeRunRef $ writeRef ref (Just r)
              Nothing, Right _ -> t.kill earlyError *> liftEff (k r)
              Just r', _       -> t.kill earlyError *> liftEff (k r')
        Thread t3 <- unsafeLaunchFuture $ runK t2 =<< attempt t1.join
        Thread t4 <- unsafeLaunchFuture $ runK t1 =<< attempt t2.join
        pure $ Canceler \err →
          parSequence_
            [ t3.kill earlyError
            , t4.kill earlyError
            , t1.kill earlyError
            , t2.kill earlyError
            ]

foreign import _pure :: forall eff a. a -> Future eff a
foreign import _map :: forall eff a b. Fn.Fn2 (a -> b) (Future eff a) (Future eff b)
foreign import _bind :: forall eff a b. Fn.Fn2 (Future eff a) (a -> Future eff b) (Future eff b)
foreign import _throwError :: forall eff a. Error -> Future eff a
foreign import _liftEff :: forall eff a. Eff eff a -> Future eff a

-- | attempt computation of Future
foreign import attempt :: forall eff a. Future eff a -> Future eff (Either Error a)

foreign import _bracket :: forall eff a b. Fn.Fn3 (Future eff a) (a -> Future eff Unit) (a -> Future eff b) (Future eff b)

foreign import makeFuture :: forall eff a. ((Either Error a -> Eff eff Unit) -> Eff eff (Canceler eff)) -> Future eff a

foreign import _delay :: forall a eff. Fn.Fn2 (Unit -> Either a Unit) Number (Future eff Unit)

foreign import _launchFuture
  :: forall eff a
  . Fn.Fn6
      (Either Error a -> Boolean)
      (Either Error a -> Error)
      (Either Error a -> a)
      (Error -> Either Error a)
      (a -> Either Error a)
      (Future eff a)
      (Eff (async :: ASYNC | eff) (Thread eff a))

unsafeFromLeft :: forall x y. Either x y -> x
unsafeFromLeft = case _ of
  Left a   -> a
  Right  _ -> unsafeCrashWith "unsafeFromLeft: Right"

unsafeFromRight :: forall x y. Either x y → y
unsafeFromRight = case _ of
  Right a -> a
  Left  _ -> unsafeCrashWith "unsafeFromRight: Left"

instance functorThread :: Functor (Thread eff) where
  map f (Thread { kill, join }) = Thread { kill, join: f <$> join }

derive instance newtypeCanceler :: Newtype (Canceler eff) _

instance semigroupCanceler :: Semigroup (Canceler eff) where
  append (Canceler c1) (Canceler c2) =
    Canceler \err -> parSequence_ [ c1 err, c2 err ]

instance monoidCanceler :: Monoid (Canceler eff) where
  mempty = nonCanceler