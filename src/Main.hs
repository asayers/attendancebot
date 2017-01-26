{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Attendance.Config
import Attendance.Monad
import Attendance.Report
import Attendance.Schedule
import Attendance.Spreadsheet
import Attendance.TimeSheet
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Except
import Data.List
import qualified Data.Text as T
import Data.Thyme
import Data.Thyme.Clock.POSIX
import Data.Thyme.Time
import Web.Slack hiding (lines)

-------------------------------------------------------------------------------

main :: IO ()
main = do
    slackConfig <- getSlackConfig
    logPath <- getCheckinLog
    botUser <- getBotUser
    annChan <- getAnnouncementChannel
    putStrLn $ "Writing data to " ++ logPath
    runAttendance slackConfig logPath blacklist timezone deadline $ do
        liftIO $ putStrLn "Established slack connection"
        -- start cron thread
        liftIO $ putStrLn "Starting job scheduler..."
        either throwM runBotSchedule (scheduledJobs annChan)
        -- run main loop
        liftIO $ putStrLn "Fetching spreadsheet data..."
        updateFromSpreadsheet =<< getAttendanceData
        liftIO $ putStrLn "Listening to events..."
        forever (getNextEvent >>= handleEvent botUser annChan)

handleEvent :: UserId -> ChannelId -> Event -> Attendance ()
handleEvent botUser annChan ev = case ev of
    ReactionAdded uid _ item_uid _ ts | item_uid == botUser ->
        checkin uid (timestampToUTCTime ts)
    Message cid (UserComment uid) msg (timestampToUTCTime -> ts) _ _ -> do
        isIM <- channelIsIM cid
        when (isIM && uid /= botUser) $ case msg of
            "active" -> markActive uid ts
            "inactive" -> markInactive uid ts
            "debug" -> dumpDebug uid =<< either throwM return (scheduledJobs annChan)
            "summary" -> sendRichIM uid "" . (:[]) =<< weeklySummary
            "spreadsheet" -> sendIM uid =<< ppSpreadsheet =<< getAttendanceData
            _ -> checkin uid ts
    ImCreated _ im -> trackUser im
    PresenceChange _ _    -> return ()  -- expected, ignore
    ReconnectUrl _        -> return ()  -- expected, ignore
    UserTyping _ _        -> return ()  -- expected, ignore
    MessageResponse _ _ _ -> return ()  -- expected, ignore
    _ -> liftIO $ print ev              -- anything else is unexpected, log it

dumpDebug :: UserId -> BotSchedule -> Attendance ()
dumpDebug uid sched = do
    ts <- getTimeSheet
    curTime <- liftIO $ getCurrentTime
    session <- getSession
    let getUsername' target = maybe "unknown" _userName $
          find (\user -> _userId user == target) (_slackUsers session)
    sendIM uid $ T.concat
        [ "```\n"
        , ppTimesheet ts curTime getUsername'
        , ppSchedule sched
        , "```"
        ]

-------------------------------------------------------------------------------
-- Helpers

timestampToUTCTime :: SlackTimeStamp -> UTCTime
timestampToUTCTime = view (slackTime . getTime . thyme . from posixTime)
