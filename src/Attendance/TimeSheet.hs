{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module contains functionality for analysing user check-ins. Only
-- the first check-in of the day is recorded, and times are represented in
-- the local time zone. cf. Attendance.CheckIn.
module Attendance.TimeSheet
    ( TimeSheet
    , newTimeSheet
    , updateTimeSheet

    , TimeSheetUpdate(..)
    , timeSheetUpdate

    , Timing(..)
    , lookupTiming

    , lateComers
    , allUsers
    , goodRunLength

      -- * Debugging
    , prettyPrintTimesheet
    ) where

import Control.Lens
import Data.AffineSpace
import Data.Function
import qualified Data.HashMap.Strict as HMS
import Data.Hashable
import Data.List
import Data.Monoid
import qualified Data.Text as T
import Data.Thyme
import Data.Thyme.Time
import Data.Time.Zones
import System.Locale
import Web.Slack hiding (lines)

-------------------------------------------------------------------------------

-- All values of type TimeOfDay are local to _tsTimeZone
data TimeSheet = TimeSheet
    { _tsCheckIns :: HMS.HashMap (UserId, Day) TimeOfDay
    , _tsHolidays :: HMS.HashMap UserId [Holiday]
    , _tsTimeZone :: TZ
    , _tsDeadline :: TimeOfDay  -- ^ When should users have checked in by?
    }

-- | A closed or right-open interval over days. Bounds are inclusive.
data Holiday
    = CompletedHoliday Day Day
    | OngoingHoliday Day
    deriving (Eq, Show)

-- TODO: Remove once liyang uploads the next version of thyme
instance Hashable Day

makeLenses ''TimeSheet

newTimeSheet :: TZ -> TimeOfDay -> TimeSheet
newTimeSheet _tsTimeZone _tsDeadline =
    let _tsCheckIns = HMS.empty
        _tsHolidays = HMS.empty
    in TimeSheet{..}

updateTimeSheet :: TimeSheetUpdate -> TimeSheet -> TimeSheet
updateTimeSheet entry ts = case entry of
    CheckIn uid time ->
        let LocalTime day tod = toLocalTime time
        in over tsCheckIns (HMS.insertWith min (uid, day) tod) ts
    MarkInactive uid time ->
        let LocalTime day _ = toLocalTime time
        in over (tsHolidays . at uid . non []) (startHoliday (day .+^ 1)) ts
    MarkActive uid time ->
        let LocalTime day _ = toLocalTime time
        in over (tsHolidays . at uid . non []) (endHoliday (day .-^ 1)) ts
  where
    toLocalTime = toThyme . utcToLocalTimeTZ (ts ^. tsTimeZone) . fromThyme

startHoliday :: Day -> [Holiday] -> [Holiday]
startHoliday start hols = case hols of
    OngoingHoliday _ : _ -> hols      -- holiday in progress; do nothing
    _ -> OngoingHoliday start : hols  -- start a holiday

endHoliday :: Day -> [Holiday] -> [Holiday]
endHoliday end hols = case hols of
    OngoingHoliday start : xs -> CompletedHoliday start end : xs
    _ -> hols                        -- no holiday in progress; do nothing

-------------------------------------------------------------------------------

data Timing
    = OnTime TimeOfDay
    | Late TimeOfDay
    | Absent
    | OnHoliday
    deriving (Eq, Ord, Show)

lookupTiming :: TimeSheet -> UserId -> Day -> Timing
lookupTiming ts uid day =
    case HMS.lookup (uid, day) (ts ^. tsCheckIns) of
        _ | isOnHoliday ts uid day     -> OnHoliday
        Just tod
            | tod < (ts ^. tsDeadline) -> OnTime tod
            | otherwise                -> Late tod
        Nothing                        -> Absent

-- TODO: We could improve performance by using the assumption that holidays
-- are ordered to short-circuit.
isOnHoliday :: TimeSheet -> UserId -> Day -> Bool
isOnHoliday ts uid day =
    anyOf (tsHolidays . ix uid . traverse) (day `isInHoliday`) ts

isInHoliday :: Day -> Holiday -> Bool
isInHoliday day hol = case hol of
    OngoingHoliday   start     -> day >= start
    CompletedHoliday start end -> day >= start && day <= end

-------------------------------------------------------------------------------

data TimeSheetUpdate
    = CheckIn UserId UTCTime
    | MarkInactive UserId UTCTime -- ^ The user is inactive as of the specified time
    | MarkActive UserId UTCTime -- ^ The user is active as of the specified time

instance Show TimeSheetUpdate where
    show entry = case entry of
        CheckIn  uid ts -> showUidTs "Check-in" uid ts
        MarkInactive uid ts -> showUidTs "Inactive" uid ts
        MarkActive   uid ts -> showUidTs "Active" uid ts
      where
        showUidTs name (Id uid) ts = unwords
            [ name <> ":", T.unpack uid, "at"
            , formatTime defaultTimeLocale "%H:%M" ts
            ]

timeSheetUpdate :: Prism' T.Text TimeSheetUpdate
timeSheetUpdate = prism' pp parse
  where
    pp entry = T.intercalate "\t" $ case entry of
        CheckIn      (Id uid) ts -> ["checkin" , review posixTime ts, uid]
        MarkActive   (Id uid) ts -> ["active"  , review posixTime ts, uid]
        MarkInactive (Id uid) ts -> ["inactive", review posixTime ts, uid]
    parse txt = case T.splitOn "\t" txt of
        [name,ts,uid] | name == "checkin"  -> CheckIn      (Id uid) <$> preview posixTime ts
        [name,ts,uid] | name == "active"   -> MarkActive   (Id uid) <$> preview posixTime ts
        [name,ts,uid] | name == "inactive" -> MarkInactive (Id uid) <$> preview posixTime ts
        _ -> Nothing

posixTime :: Prism' T.Text UTCTime
posixTime = prism' pp parse
  where
    pp = T.pack . formatTime defaultTimeLocale "%s"
    parse = parseTime defaultTimeLocale "%s" . T.unpack

-------------------------------------------------------------------------------

-- userHistory :: UserId -> TimeSheet -> HMS.HashMap Day TimeOfDay
-- userHistory target timeSheet =
--     mapKeys snd $ HMS.filterWithKey (\(uid,_) _ -> uid == target) timeSheet

-- todaysCheckIns :: TimeSheet -> IO (HMS.HashMap UserId TimeOfDay)
-- todaysCheckIns timeSheet = do
--     today <- localDay . zonedTimeToLocalTime <$> getZonedTime
--     return $ mapKeys fst $ HMS.filterWithKey (\(_,day) _ -> day == today) timeSheet

lateComers :: TimeSheet -> Day -> [UserId]
lateComers ts day = filter late (allUsers ts)
  where
    late uid = case lookupTiming ts uid day of
        OnTime _ -> False
        Late _ -> True
        Absent -> True
        OnHoliday -> False

allUsers :: TimeSheet -> [UserId]
allUsers = nub . map fst . HMS.keys . view tsCheckIns

goodRunLength :: TimeSheet -> Day -> Int
goodRunLength ts start =
    let isGood = null . lateComers ts
    in length $ takeWhile isGood $ map (start .-^) [0..]

-------------------------------------------------------------------------------
-- For debugging

prettyPrintTimesheet :: TimeSheet -> UTCTime -> (UserId -> T.Text) -> T.Text
prettyPrintTimesheet ts curTime getUsername = T.unlines $
    [ "Deadline: " <> T.pack (show (ts ^. tsDeadline))
    , "Current time: " <> T.pack (show (utcToLocalTimeTZ (ts ^. tsTimeZone) (fromThyme curTime)))
    ] ++ concatMap ppCheckIns days
    ++ ppHolidays
  where
    days = take 10 $ sort $ nub $ map snd $ HMS.keys $ ts ^. tsCheckIns
    ppCheckIns d =
        ("Check-ins for " <> tshow d <> ":") :
        [ ppCheckIn uid day time
        | ((uid, day), time) <- sortBy (compare `on` snd) $ HMS.toList (ts ^. tsCheckIns)
        , day == d
        ]
    ppCheckIn uid day time = mconcat
        [ "  " <> getUsername uid <> " at "
        , tshow time
        , " (" <> ppTiming (lookupTiming ts uid day) <> ")"
        ]
    -- TODO: only show recent
    ppHolidays =
        "Holidays: " :
        [ ppHoliday uid hol
        | (uid, hols) <- HMS.toList $ ts ^. tsHolidays
        , hol <- hols
        ]
    ppHoliday uid hol = "  " <> getUsername uid <> case hol of
        OngoingHoliday start -> " from " <> tshow start <> " to present"
        CompletedHoliday start end -> " from " <> tshow start <> " to " <> tshow end
    ppTiming = \case
        OnTime _ -> "on time"
        Late _ -> "late"
        Absent -> "absent"
        OnHoliday -> "on holiday"

    tshow :: Show a => a -> T.Text
    tshow = T.pack . show
