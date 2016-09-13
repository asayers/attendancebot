{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Attendance.Monad
import Attendance.Report
import Attendance.Spreadsheet
import Control.Concurrent
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Trans.Control
import Data.Maybe
import qualified Data.Text as T
import Data.Thyme
import Data.Thyme.Clock.POSIX
import Data.Thyme.Time
import Data.Time.Zones
import Data.Time.Zones.All
import System.Cron
import System.Environment
import Web.Slack hiding (lines)

-------------------------------------------------------------------------------

main :: IO ()
main = do
    slackConfig <- getSlackConfig
    logPath <- getCheckinLog
    putStrLn $ "Writing data to " ++ logPath
    runAttendance slackConfig logPath blacklist timezone deadline $ do
        liftIO $ putStrLn "Established slack connection"
        -- start cron thread
        liftIO $ putStrLn "Starting job scheduler..."
        either throwM (void . liftBaseDiscard forkIO . runJobs) scheduledJobs
        -- run main loop
        liftIO $ putStrLn "Fetching spreadsheet data..."
        updateFromSpreadsheet =<< getAttendanceData
        liftIO $ putStrLn "Listening to events..."
        forever (getNextEvent >>= handleEvent)

handleEvent :: Event -> Attendance ()
handleEvent ev = case ev of
    ReactionAdded uid _ item_uid _ ts | item_uid == user_me ->
        checkin uid (timestampToUTCTime ts)
    Message cid (UserComment uid) msg (timestampToUTCTime -> ts) _ _ -> do
        isIM <- channelIsIM cid
        when (isIM && uid /= user_me) $ case msg of
            "active" -> markActive uid ts
            "inactive" -> markInactive uid ts
            "debug" -> dumpDebug uid =<< either throwM return scheduledJobs
            "summary" -> sendRichIM uid "" . (:[]) =<< weeklySummary
            "spreadsheet" -> sendIM uid =<< ppSpreadsheet =<< getAttendanceData
            _ -> checkin uid ts
    ImCreated _ im -> trackUser im
    PresenceChange _ _ -> return ()  -- expected, ignore
    ReconnectUrl _     -> return ()  -- expected, ignore
    _ -> liftIO $ print ev           -- anything else is unexpected, log it

-------------------------------------------------------------------------------
-- Configuration

getSlackConfig :: IO SlackConfig
getSlackConfig =
    maybe (error "SLACK_API_TOKEN not set") SlackConfig <$> lookupEnv "SLACK_API_TOKEN"

getCheckinLog :: IO FilePath
getCheckinLog =
    fromMaybe (error "ATTENDANCE_LOG not set") <$> lookupEnv "ATTENDANCE_LOG"

-- | Users which we want to ignore
blacklist :: [UserId]
blacklist =
    [ Id "USLACKBOT" -- @slackbot
    ]

timezone :: TZ
timezone = tzByLabel Asia__Tokyo

deadline :: TimeOfDay
deadline = TimeOfDay 9 0 (fromSeconds' 0)  -- 9am JST

-- TODO: Get this from session
user_me :: UserId
user_me = Id ""  -- @attendancebot

channel_announce :: ChannelId
channel_announce = Id ""

-------------------------------------------------------------------------------

scheduledJobs :: Either ScheduleError [Job Attendance]
scheduledJobs = sequence
    [ mkJob "45 23 * * 0-4" remindMissing         -- 8:45 mon-fri
    , mkJob "55 23 * * 0-4" sendDailySummary      -- 8:55 mon-fri
    , mkJob "31 3 * * 5"    sendWeeklySummary     -- midday on friday
    , mkJob "00 20 * * 0-4" downloadSpreadsheet   -- 5:00 mon-fri
    ]

sendDailySummary :: Attendance ()
sendDailySummary = sendMessage channel_announce =<< dailySummary

remindMissing :: Attendance ()
remindMissing = mapM_ (uncurry sendIM) =<< missingReport

sendWeeklySummary :: Attendance ()
sendWeeklySummary = do
    attachment <- weeklySummary
    ret <- sendRichMessage channel_announce "" [attachment]
    either (liftIO . putStrLn . T.unpack) return ret

downloadSpreadsheet :: Attendance ()
downloadSpreadsheet = updateFromSpreadsheet =<< getAttendanceData

-------------------------------------------------------------------------------
-- Helpers

timestampToUTCTime :: SlackTimeStamp -> UTCTime
timestampToUTCTime = view (slackTime . getTime . thyme . from posixTime)
