module Benchmark.Main where

import Prelude

import Control.Monad.Aff (Aff, runAff, forkAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Eff.Exception (EXCEPTION, throwException)
import Control.Monad.Aff.AVar as AV
import Control.Monad.Aff.MVar as MV
import Control.Monad.Rec.Class (forever)

import Data.Foldable (for_)

import Benchotron.Core (Benchmark, BenchEffects, benchFn, mkBenchmark)
import Benchotron.UI.Console (runSuite)

import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (Gen, vectorOf)

putVarBenchmark :: Benchmark
putVarBenchmark = mkBenchmark $
  { slug: "put-var"
  , title: "Put value to var (" <> show inputsPerSize <> " input per size)"
  , sizes: [1, 2, 3, 4, 5, 10, 20, 30, 40, 50, 100, 250, 500, 1000]
  , sizeInterpretation: "Number of vars"
  , inputsPerSize: inputsPerSize
  , gen: randomArray
  , functions: [ benchFn "AVar" (unsafePerformEff <<< handleAff <<< putAffAVar)
               , benchFn "MVar" (unsafePerformEff <<< handleAff <<< putAffMVar)
               ]
  }
  where
  inputsPerSize :: Int
  inputsPerSize = 100

  putAffAVar xs = do
    av <- AV.makeVar
    for_ xs \x -> AV.putVar av x

  putAffMVar xs = do
    mv <- MV.newEmptyMVar
    for_ xs \x -> MV.putMVar mv x

putTakeVarBenchmark :: Benchmark
putTakeVarBenchmark = mkBenchmark $
  { slug: "put-take-var"
  , title: "Put and take value from var (" <> show inputsPerSize <> " input per size)"
  , sizes: [1, 2, 3, 4, 5, 10, 20, 30, 40, 50, 100, 250, 500, 1000]
  , sizeInterpretation: "Number of vars"
  , inputsPerSize: inputsPerSize
  , gen: randomArray
  , functions: [ benchFn "AVar" (unsafePerformEff <<< handleAff <<< putTakeAffAVar)
               , benchFn "MVar" (unsafePerformEff <<< handleAff <<< putTakeAffMVar)
               ]
  }
  where
  inputsPerSize :: Int
  inputsPerSize = 100

  putTakeAffAVar xs = do
    av <- AV.makeVar
    _ <- forkAff $ for_ xs \x -> AV.putVar av x
    void $ forkAff $ forever $ do
      _ <- AV.takeVar av
      pure unit

  putTakeAffMVar xs = do
    mv <- MV.newEmptyMVar
    _ <- forkAff $ for_ xs \x -> MV.putMVar mv x
    void $ forkAff $ forever $ do
      _ <- MV.takeMVar mv
      pure unit

randomArray :: forall eff. Int -> Gen (Array Number)
randomArray = flip vectorOf arbitrary

handleAff
  :: forall eff a
   . Aff (exception :: EXCEPTION | eff) a
  -> Eff (exception :: EXCEPTION | eff) Unit
handleAff = void <<< runAff throwException (const (pure unit))

main = runSuite
  [ putVarBenchmark
  , putTakeVarBenchmark
  ]
