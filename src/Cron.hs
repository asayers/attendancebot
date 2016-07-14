
module Cron
    ( CronTab
    , CronJob(..)
    , runCron

    , TimeOfDay
    , hrs
    , mins
    ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe
import Data.Time.Clock

type CronTab m = [CronJob m]
data CronJob m = CronJob { tod :: TimeOfDay, job :: m () }

runCron :: MonadIO m => CronTab m -> m ()
runCron crontab = forever $ do
    curTime <- liftIO $ utctDayTime <$> getCurrentTime
    let (past, future) = span (\cj -> tod cj < curTime) crontab'
    case listToMaybe (future ++ past) of
        Just nextJob -> do
            liftIO $ threadDelay $ round (todDiff curTime (tod nextJob)) * 1000000
            job nextJob
             -- sleep for 1 second to to avoid repeating jobs due to bad
             -- clock resolution
            liftIO $ threadDelay 1000000
        Nothing -> liftIO $ threadDelay 1000000
  where
    crontab' = sortBy (compare `on` tod) crontab

-------------------------------------------------------------------------------
-- Time of day

type TimeOfDay = {- a UTC time-of-day -}DiffTime

hrs :: Integer -> TimeOfDay
hrs x = secondsToDiffTime $ 60*60*x

mins :: Integer -> TimeOfDay
mins x = secondsToDiffTime $ 60*x

-- | The length of the time period beginning at tod1 and ending at tod2.
todDiff :: TimeOfDay -> TimeOfDay -> DiffTime
todDiff tod1 tod2 = case tod2 - tod1 of
    x | x < 0      -> todDiff tod1 (tod2 + hrs 24)
      | x > hrs 24 -> todDiff tod1 (tod2 - hrs 24)
      | otherwise  -> x

