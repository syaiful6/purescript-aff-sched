module Test.Main where

import Prelude
import Control.Monad.Aff (Aff, runAff, delay)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, throwException)
import Control.Monad.Eff.Ref (Ref, newRef, readRef, modifyRef', modifyRef)

import Control.Monad.Aff.Schedule.Effects (ScheduleEff)
import Control.Monad.Aff.Schedule.AutoUpdate (mkAutoUpdate, UpdateSettings(..))
import Control.Monad.Aff.Schedule.Reaper (ReaperSetting(..), mkReaper, mkListAction, reaperAdd)

import Data.Newtype (wrap)
import Data.List (List(Nil), (..), (:), null)
import Data.Foldable (for_)
import Data.Traversable (traverse)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Math (abs)
import Unsafe.Coerce (unsafeCoerce)


type TestEff e = ScheduleEff (console :: CONSOLE, exception :: EXCEPTION | e)

testAction
  :: forall eff
   . List (Tuple Number (Ref Number))
  -> Aff (TestEff eff) (List (Tuple Number (Ref Number)) -> List (Tuple Number (Ref Number)))
testAction = mkListAction $ \(Tuple i ref) -> do
  liftEff $ modifyRef ref (add 1.00)
  pure $ if i > 1.00 then Just (Tuple (i - 1.00) ref) else Nothing

testReaper :: forall e. Aff (TestEff e) Unit
testReaper = do
  reaper <- mkReaper (ReaperSetting
              { action: testAction
              , delay: wrap 1000.00
              , cons: (:)
              , isNull: null
              , empty: Nil
              })
  let
    is = (unsafeCoerce (1..50)) :: List Number
    mkTestCase i = do
      ref <- liftEff $ newRef 0.00
      let expected = (abs i `mod` 10.00) + 1.00
      reaperAdd reaper (Tuple expected ref)
      pure (Tuple expected ref)
    test (Tuple expected ref) = do
      actual <- liftEff $ readRef ref
      when (actual /= expected) do
        liftEff $ log ("someting wrong happened, expected: " <> show expected <> ", but got " <> show actual)
  testCases <- traverse mkTestCase is
  delay (wrap 2000.00)
  for_ testCases test

testAutoUpdate :: forall e. Aff (TestEff e) Unit
testAutoUpdate = do
  ref <- liftEff $ newRef 0
  let
    update = liftEff $ modifyRef' ref \s ->
      let i = s + 1
      in { state: i, value: i}
  next <- mkAutoUpdate (UpdateSettings (wrap 1000.00) update)
  for_ (1..10) \i -> do
    j <- next
    when (i == j && i /= 1) do
      liftEff $ log "someting wrong happened"
  delay (wrap 3000.00)
  last1 <- liftEff $ readRef ref
  delay (wrap 2000.00)
  last2 <- liftEff $ readRef ref
  when (last1 /= last2) do
    liftEff $ log "someting wrong happened"

main :: forall e. Eff (TestEff e) Unit
main = void $ runAff throwException (const (pure unit)) $ do
  liftEff $ log "test auto update module"
  testAutoUpdate
  liftEff $ log "test reaper module"
  testReaper