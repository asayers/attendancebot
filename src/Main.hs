{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Attendance.Config
import Attendance.Monad
import Attendance.Report
import Attendance.Spreadsheet
import Control.Concurrent
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Trans.Control
import qualified Data.Text as T
import Data.Thyme
import Data.Thyme.Clock.POSIX
import Data.Thyme.Time
import System.Cron
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
        either throwM (void . liftBaseDiscard forkIO . runJobs) (scheduledJobs annChan)
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

-------------------------------------------------------------------------------

scheduledJobs :: ChannelId -> Either ScheduleError [Job Attendance]
scheduledJobs annChan = sequence
    [ mkJob "30 23 * * 0-4" remindMissing                  -- 8:30 mon-fri
    , mkJob "45 23 * * 0-4" (sendDailySummary annChan)     -- 8:45 mon-fri
    , mkJob "31 3 * * 5"    (sendWeeklySummary annChan)    -- midday on friday
    , mkJob "00 20 * * 0-4" downloadSpreadsheet            -- 5:00 mon-fri
    ]

sendDailySummary :: ChannelId -> Attendance ()
sendDailySummary annChan = sendMessage annChan =<< dailySummary

remindMissing :: Attendance ()
remindMissing = mapM_ (uncurry sendIM) =<< missingReport

sendWeeklySummary :: ChannelId -> Attendance ()
sendWeeklySummary annChan = do
    attachment <- weeklySummary
    ret <- sendRichMessage annChan "" [attachment]
    either (liftIO . putStrLn . T.unpack) return ret

downloadSpreadsheet :: Attendance ()
downloadSpreadsheet = updateFromSpreadsheet =<< getAttendanceData

-------------------------------------------------------------------------------
-- Helpers

timestampToUTCTime :: SlackTimeStamp -> UTCTime
timestampToUTCTime = view (slackTime . getTime . thyme . from posixTime)
