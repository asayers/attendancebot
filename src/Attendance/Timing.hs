{-# LANGUAGE LambdaCase #-}

module Attendance.Timing
    ( Timing(..)
    , isOnTime, isLate, isAbsent, isOnHoliday
    , goodTiming
    , timingScore
    ) where

import Control.Lens
import Data.Thyme

data Timing
    = OnTime TimeOfDay TimeOfDay
    | Late TimeOfDay TimeOfDay
    | Absent TimeOfDay
    | OnHoliday
    deriving (Eq, Ord, Show)

isOnTime, isLate, isAbsent, isOnHoliday :: Timing -> Bool
isOnTime    = \case OnTime{}    -> True; _ -> False
isLate      = \case Late{}      -> True; _ -> False
isAbsent    = \case Absent{}    -> True; _ -> False
isOnHoliday = \case OnHoliday{} -> True; _ -> False

goodTiming :: Timing -> Bool
goodTiming timing = case timing of
    OnTime _ _ -> True; Late _ _ -> False; Absent _ -> False; OnHoliday -> True

-- | Must range between -1 and 1.
timingScore :: Timing -> Double
timingScore timing = case timing of
    OnHoliday -> 0
    OnTime _ _ -> 1
    Late dl ci -> negate (lateness dl ci)   -- lerp between 0 and -1
    Absent _ -> -1
  where
    reallyLate = 15 * 60  -- 15 minutes is really late
    lateness dl ci = min 1 $ max 0 $ timeOfDayDiff ci dl / reallyLate
    timeOfDayDiff t1 t2 =
        toSeconds (t1 ^. from timeOfDay) - toSeconds (t2 ^. from timeOfDay)
