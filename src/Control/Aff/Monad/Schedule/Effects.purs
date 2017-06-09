module Control.Monad.Aff.Schedule.Effects
  ( ScheduleEff
  ) where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Ref (REF)

type ScheduleEff r = (avar :: AVAR, ref :: REF | r)