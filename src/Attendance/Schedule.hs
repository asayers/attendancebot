{-# LANGUAGE OverloadedStrings #-}

module Attendance.Schedule
    ( CronJob
    , mkJob
    , runJobs
    , prettyPrintJobs
    ) where

import Control.Monad.Except
import Control.Monad.State
import Data.Attoparsec.Text
import Data.Monoid
import qualified Data.Text as T
import System.Cron.Parser
import System.Cron.Schedule
import System.Cron.Types

-------------------------------------------------------------------------------
-- Scheduled operations

data CronJob a = CronJob CronSchedule a

mkJob :: T.Text -> a -> CronJob a
mkJob schedStr j =
    case parseOnly cronSchedule schedStr of
        Left e -> error (show e)
        Right sched -> CronJob sched j

runJobs :: (a -> IO ()) -> [CronJob a] -> IO ()
runJobs runJob jobs =
    void $ execSchedule schedule
  where
    schedule :: Schedule ()
    schedule = ScheduleT $ put (map mkJob' jobs)
      where
        mkJob' (CronJob sched act) = Job sched (runJob act)

prettyPrintJobs :: (a -> T.Text) -> [CronJob a] -> T.Text
prettyPrintJobs pp = T.unlines . ("Scheduled jobs:":) . map ppJob
  where
    ppJob (CronJob sched j) = "  " <> T.pack (show sched) <> ": " <> pp j
