{-# LANGUAGE OverloadedStrings #-}

module AtnBot.Schedule
    ( BotSchedule
    , runBotSchedule
    , scheduledJobs
    , ppSchedule
    ) where

import AtnBot.Actions
import AtnBot.Monad
import Control.Concurrent
import Control.Monad.Except
import Control.Monad.Trans.Control
import Data.Monoid
import qualified Data.Text as T
import System.Cron

type BotSchedule = [(T.Text, Job AtnBot)]

runBotSchedule :: BotSchedule -> AtnBot ()
runBotSchedule = void . liftBaseDiscard forkIO . runJobs . map snd

scheduledJobs :: Either ScheduleError BotSchedule
scheduledJobs = sequence
    [ (,) "remind absent people"  <$> mkJob "30 23 * * 0-4" remindMissing       -- 8:30 mon-fri
    , (,) "send daily summary"    <$> mkJob "45 23 * * 0-4" sendDailySummary    -- 8:45 mon-fri
    , (,) "send weekly summary"   <$> mkJob "30 00 * * 5"    sendWeeklySummary  -- 9:30 on friday
    , (,) "download spreadsheet"  <$> mkJob "00 20 * * 0-4" updateFromSpreadsheet -- 5:00 mon-fri
    ]

ppSchedule :: BotSchedule -> T.Text
ppSchedule = T.unlines . ("[Scheduled jobs]":) . map ppJob
  where
    ppJob (name, Job sched _) = T.pack (show sched) <> ": " <> name
