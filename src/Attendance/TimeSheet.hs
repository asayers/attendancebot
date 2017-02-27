{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module contains functionality for analysing user check-ins. Only
-- the first check-in of the day is recorded, and times are represented in
-- the user's local time zone.
module Attendance.TimeSheet
    ( TimeSheet
    , defaultTimeSheet

      -- * Modifying
    , checkIn
    , markHoliday
    , setTimeZone

      -- * Querying
    , getTiming
    , allDays

      -- * Analysis
    , ratio
    , score

      -- * Debug
    , ppTimesheet
    ) where

import Attendance.Calendar
import Attendance.Timing
import Control.Lens
import Data.AffineSpace
import Data.Function
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMS
import Data.List
import Data.Monoid
import qualified Data.Text as T
import Data.Thyme
import Data.Thyme.Time
import Data.Time.Zones
import Data.Time.Zones.All

data TimeSheet = TimeSheet
    { _tsCheckIns :: HashMap Day TimeOfDay
    , _tsCalendar :: Calendar
    , _tsTimeZone :: TZ
    } deriving (Eq, Show)

makeLenses ''TimeSheet

defaultTimeSheet :: TZ -> TimeOfDay -> TimeSheet
defaultTimeSheet tz deadline = TimeSheet
    { _tsCheckIns = HMS.empty
    , _tsCalendar = weekendsOff deadline
    , _tsTimeZone = tz
    }

checkIn :: UTCTime -> TimeSheet -> TimeSheet
checkIn time ts =
    over tsCheckIns (HMS.insertWith min day tod) ts
  where
    LocalTime day tod = toThyme $ utcToLocalTimeTZ (ts ^. tsTimeZone) $ fromThyme time

-- | Convenience wrapper around 'markException'
markHoliday :: Day -> TimeSheet -> TimeSheet
markHoliday day = over tsCalendar $ markException day "On holiday" NotExpected

-- | FIXME: Change all internal TimeOfDay values
setTimeZone :: TZLabel -> TimeSheet -> TimeSheet
setTimeZone tz = tsTimeZone .~ (tzByLabel tz)

getTiming :: TimeSheet -> Day -> Timing
getTiming ts day =
    case (ts ^. tsCalendar . to expectation $ day, ts ^? tsCheckIns . ix day) of
        (NotExpected, _) -> OnHoliday
        (ExpectedAt deadline, Nothing) -> Absent deadline
        (ExpectedAt deadline, Just checkin)
            | checkin < deadline -> OnTime deadline checkin
            | otherwise ->  Late deadline checkin

allDays :: TimeSheet -> Day -> [Day]
allDays ts today =
    case sort $ HMS.keys $ ts ^. tsCheckIns of
        [] -> []
        (first:_) -> [first..today]

-------------------------------------------------------------------------------
-- Analysis

ratio :: TimeSheet -> Day -> Double
ratio ts today =
    let (goodDays, badDays) = partition goodTiming $ map (getTiming ts) (allDays ts today)
    in fromIntegral (length goodDays) / fromIntegral (length $ badDays)

score :: TimeSheet -> Day -> Double
score ts today = mkScore (halflife today) ts today

-- | A score between 0 and 1
mkScore :: (Day -> Double) -> TimeSheet -> Day -> Double
mkScore weight ts today = 0.5 + (scaledScore / 2)
  where
    -- ranges between -1 and 1
    scaledScore = myScore / bestScore
    -- ranges between -bestScore and bestScore
    myScore = sum $ map (\day -> timingScore (getTiming ts day) * weight day) days
    bestScore = sum (map weight days)
    days = allDays ts today

halflife :: Day -> Day -> Double
halflife today day = r ^ (today .-. day)   -- weight by halflife decay
  where r = 0.5 ** (1/7)    -- halflife of 7 days

-------------------------------------------------------------------------------
-- Debugging

ppTimesheet :: TimeSheet -> T.Text
ppTimesheet ts = T.unlines $ concatMap ppCheckIns days -- ++ ppHolidays'
  where
    days = reverse $ take 5 $ reverse $ sort $ nub $ HMS.keys $ ts ^. tsCheckIns
    ppCheckIns d =
        ("[Check-ins for " <> tshow d <> "]") :
        [ ppCheckIn day time
        | (day, time) <- sortBy (compare `on` snd) $ HMS.toList (ts ^. tsCheckIns)
        , day == d
        ] ++ [""]
    ppCheckIn day time = tshow time <> " (" <> ppTiming (getTiming ts day) <> ")"
    -- ppHolidays' =
    --     "[Holidays]" :
    --     [ getUsername uid <> " " <> T.pack (ppHoliday hol)
    --     | (uid, hols) <- HMS.toList $ ts ^. tsHolidays
    --     , hol <- toList hols
    --     , _hUntil hol >= curTime ^. _utctDay
    --     ] ++ [""]
    ppTiming t = case t of
        OnTime _ _ -> "on time"
        Late _ _ -> "late"
        Absent _ -> "absent"
        OnHoliday -> "on holiday"
    tshow :: Show a => a -> T.Text
    tshow = T.pack . show
