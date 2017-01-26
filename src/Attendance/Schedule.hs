{-# LANGUAGE OverloadedStrings #-}

module Attendance.Schedule
    ( BotSchedule
    , runBotSchedule
    , scheduledJobs
    , ppSchedule
    ) where

import Attendance.Monad
import Attendance.Report
import Attendance.Spreadsheet
import Control.Concurrent
import Control.Monad.Except
import Control.Monad.Trans.Control
import Data.Monoid
import qualified Data.Text as T
import System.Cron
import Web.Slack hiding (lines)

type BotSchedule = [(T.Text, Job Attendance)]

runBotSchedule :: BotSchedule -> Attendance ()
runBotSchedule = void . liftBaseDiscard forkIO . runJobs . map snd

scheduledJobs :: ChannelId -> Either ScheduleError BotSchedule
scheduledJobs annChan = sequence
    [ (,) "remindMissing"       <$> mkJob "30 23 * * 0-4" remindMissing                 -- 8:30 mon-fri
    , (,) "sendDailySummary"    <$> mkJob "45 23 * * 0-4" (sendDailySummary annChan)    -- 8:45 mon-fri
    , (,) "sendWeeklySummary"   <$> mkJob "31 3 * * 5"    (sendWeeklySummary annChan)   -- midday on friday
    , (,) "downloadSpreadsheet" <$> mkJob "00 20 * * 0-4" downloadSpreadsheet           -- 5:00 mon-fri
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

ppSchedule :: BotSchedule -> T.Text
ppSchedule = T.unlines . ("Scheduled jobs:":) . map ppJob
  where
    ppJob (name, Job sched _) = "    " <> T.pack (show sched) <> ": " <> name
