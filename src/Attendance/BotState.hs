{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Attendance.BotState
    ( BotState
    , newBotState

    , BotStateUpdate(..)
    , botStateUpdate_
    , updateBotState

    , userTimeSheet
    , timings
    , lateComers
    , holidayMakers
    , allUsers
    , goodRunLength

      -- * Debugging
    , ppBotState
    ) where

import Attendance.TimeSheet
import Attendance.Timing
import Control.Lens
import Data.AffineSpace
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMS
import Data.List
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T
import Data.Thyme
import Data.Time.Zones.All
import System.Locale
import Web.Slack hiding (lines)

data BotState = BotState
    { _bsTimeSheets :: HashMap UserId TimeSheet
    , _bsDefaultTimeSheet :: TimeSheet
    }

makeLenses ''BotState

newBotState :: TimeSheet -> BotState
newBotState defTimeSheet = BotState HMS.empty defTimeSheet

-------------------------------------------------------------------------------
-- Serialisable state updates

data BotStateUpdate
    = CheckIn UserId UTCTime
    | MarkHoliday UserId Day Double
    | SetTimeZone UserId TZLabel
    deriving (Show)

updateBotState :: BotStateUpdate -> BotState -> BotState
updateBotState bsu bs = bs & case bsu of
    CheckIn uid ts        -> over (timeSheet uid) $ checkIn ts
    MarkHoliday uid day _ -> over (timeSheet uid) $ markHoliday day
    SetTimeZone uid tz    -> over (timeSheet uid) $ setTimeZone tz
  where
    timeSheet uid = bsTimeSheets . at uid . non (bs ^. bsDefaultTimeSheet)

-- instance Show BotStateUpdate where
--     show entry = case entry of
--         CheckIn (Id uid) ts ->
--             "Check-in: " <> T.unpack uid <> " at "
--             <> formatTime defaultTimeLocale "%H:%M" ts
--         MarkHoliday (Id uid) day amt ->
--             "Holiday: " <> T.unpack uid <>
--             " on " <> show day <>
--             "(" <> show amt <> ")"
--         SetTimeZone (Id uid) tz ->
--             "Set timezone: " <> T.unpack uid <> " to " <> T.unpack (tzLabel_ # tz)

botStateUpdate_ :: Prism' T.Text BotStateUpdate
botStateUpdate_ = prism' pp parse
  where
    pp entry = T.intercalate "\t" $ case entry of
        CheckIn      (Id uid) ts      -> ["checkin" , posixTime_ # ts, uid]
        MarkHoliday  (Id uid) day amt -> ["holiday" , day_ # day, uid, double_ # amt]
        SetTimeZone  (Id uid) tz      -> ["timezone", uid, tzLabel_ # tz]
    parse txt = case T.splitOn "\t" txt of
        [cmd,ts,uid]      | cmd == "checkin"  -> CheckIn     (Id uid) <$> ts ^? posixTime_
        [cmd,day,uid,amt] | cmd == "holiday"  -> MarkHoliday (Id uid) <$> day ^? day_ <*> amt ^? double_
        [cmd,uid,tz]      | cmd == "timezone" -> SetTimeZone (Id uid) <$> tz ^? tzLabel_
        _ -> Nothing

tzLabel_ :: Prism' T.Text TZLabel
tzLabel_ = prism' (T.decodeUtf8 . toTZName) (fromTZName . T.encodeUtf8)

posixTime_ :: Prism' T.Text UTCTime
posixTime_ = prism' pp parse
  where
    pp = T.pack . formatTime defaultTimeLocale "%s"
    parse = parseTime defaultTimeLocale "%s" . T.unpack

day_ :: Prism' T.Text Day
day_ = prism' pp parse
  where
    pp = T.pack . formatTime defaultTimeLocale "%Y-%m-%d"
    parse = parseTime defaultTimeLocale "%Y-%m-%d" . T.unpack

double_ :: Prism' T.Text Double
double_ = prism' pp parse
  where
    pp = T.pack . show
    parse = either (const Nothing) (Just . fst) . T.double

-------------------------------------------------------------------------------
-- Querying

userTimeSheet :: BotState -> UserId -> TimeSheet
userTimeSheet bs uid = bs ^. bsTimeSheets . at uid . non (bs ^. bsDefaultTimeSheet)

allUsers :: BotState -> [UserId]
allUsers = nub . HMS.keys . view bsTimeSheets

timings :: BotState -> Day -> [(UserId, Timing)]
timings bs day = map f $ HMS.toList (bs ^. bsTimeSheets)
  where f (uid, ts) = (uid, getTiming ts day)

holidayMakers :: BotState -> Day -> [UserId]
holidayMakers bs day = filterByTiming (== OnHoliday) day bs

lateComers :: BotState -> Day -> [UserId]
lateComers bs day = filterByTiming (not . goodTiming) day bs

filterByTiming :: (Timing -> Bool) -> Day -> BotState -> [UserId]
filterByTiming p day bs = HMS.foldlWithKey' f [] (bs ^. bsTimeSheets)
  where f xs uid ts = if p (getTiming ts day) then uid:xs else xs

goodRunLength :: BotState -> Day -> Int
goodRunLength ts start =
    let isGood = null . lateComers ts
    in length $ takeWhile isGood $ map (start .-^) [0..]

-- -- | A score between 0 and 1
-- userScore :: Day -> UserId -> TimeSheet -> Double
-- userScore today uid ts = 0.5 + (trueScore / 2)
--   where
--     trueScore = (sum $ map score $ allDays ts) / best -- ranges between -1 and 1
--     score day = timingScore (lookupTiming ts uid day) * weight day
--     lateness t = min 1 $ max 0 $ timeOfDayDiff t (_tsDeadline ts) / reallyLate
--     timeOfDayDiff t1 t2 = toSeconds (t1 ^. from timeOfDay) - toSeconds (t2 ^. from timeOfDay)
--     weight day = r ^ (today .-. day)   -- weight by halflife decay
--     best = (1 - r ^ (length $ allDays ts)) / (1 - r) -- the best possible score
--     r = 0.5 ** (1/7)    -- halflife of 7 days
--     reallyLate = 15 * 60  -- 15 minutes is really late
--     timingScore timing = case timing of
--         OnHoliday -> 0
--         OnTime _ -> 1
--         Late t -> negate (lateness t)   -- lerp between 0 and -1
--         Absent -> -1


-------------------------------------------------------------------------------
-- For debugging

ppBotState :: BotState -> UTCTime -> (UserId -> T.Text) -> T.Text
ppBotState = undefined

-- ppTimesheet :: TimeSheet -> UTCTime -> (UserId -> T.Text) -> T.Text
-- ppTimesheet ts curTime getUsername = T.unlines $
--     [ "Deadline: " <> T.pack (show (ts ^. tsDeadline))
--     , "Current time: " <> T.pack (show (utcToLocalTimeTZ (ts ^. tsTimeZone) (fromThyme curTime)))
--     , ""
--     ] ++ concatMap ppCheckIns days
--     ++ ppHolidays'
--   where
--     days = reverse $ take 5 $ reverse $ sort $ nub $ map snd $ HMS.keys $ ts ^. tsCheckIns
--     ppCheckIns d =
--         ("[Check-ins for " <> tshow d <> "]") :
--         [ ppCheckIn uid day time
--         | ((uid, day), time) <- sortBy (compare `on` snd) $ HMS.toList (ts ^. tsCheckIns)
--         , day == d
--         ] ++ [""]
--     ppCheckIn uid day time = mconcat
--         [ getUsername uid <> " at "
--         , tshow time
--         , " (" <> ppTiming (lookupTiming ts uid day) <> ")"
--         ]
--     ppHolidays' =
--         "[Holidays]" :
--         [ getUsername uid <> " " <> T.pack (ppHoliday hol)
--         | (uid, hols) <- HMS.toList $ ts ^. tsHolidays
--         , hol <- toList hols
--         , _hUntil hol >= curTime ^. _utctDay
--         ] ++ [""]
--     ppTiming = \case
--         OnTime _ -> "on time"
--         Late _ -> "late"
--         Absent -> "absent"
--         OnHoliday -> "on holiday"

--     tshow :: Show a => a -> T.Text
--     tshow = T.pack . show
