{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Attendance.Monad
    ( AttnH, withAttnH
    , Attendance, runAttendance

      -- * Modifying
    , checkin
    , markInactive
    , markActive
    , trackUser

      -- * Querying
    , getTimeSheet
    , getTrackedUsers
    , channelIsIM

      -- * Slack
    , sendIM
    ) where

import Attendance.Log
import Attendance.TimeSheet
import Attendance.UserTracker (TrackerHandle, newTrackerHandle)
import qualified Attendance.UserTracker as UT
import Control.Monad.Reader
import qualified Data.Text as T
import Data.Thyme
import Data.Time.Zones
import Web.Slack.Handle (SlackHandle, withSlackHandle, getSession)
import Web.Slack.Monad hiding (getSession)

newtype Attendance a = Attendance (ReaderT AttnH IO a)
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadSlack Attendance where
    askSlackHandle = Attendance $ slackH <$> ask

data AttnH = AttnH
    { slackH :: SlackHandle
    , trackerH :: TrackerHandle
    , stateH :: LogHandle TimeSheetUpdate TimeSheet
    }

withAttnH :: SlackConfig -> FilePath -> [UserId] -> TZ -> TimeOfDay -> (AttnH -> IO a) -> IO a
withAttnH conf logPath blacklist tz deadline fn = withSlackHandle conf $ \slackH -> do
    let timeSheet = newTimeSheet tz deadline
    stateH <- newLogHandle updateTimeSheet timeSheetUpdate timeSheet logPath
    trackerH <- newTrackerHandle (getSession slackH) blacklist
    fn AttnH{..}

runAttendance :: AttnH -> Attendance a -> IO a
runAttendance attnH (Attendance ma) = do
    runReaderT ma attnH

getAttnH :: Attendance AttnH
getAttnH = Attendance ask

-------------------------------------------------------------------------------
-- TimeSheet

checkin :: UserId -> UTCTime -> Attendance ()
checkin uid ts = do
    modifyTimeSheet $ CheckIn uid ts
    sendIM uid "Your attendance has been noted. Have a good day!"

markInactive :: UserId -> UTCTime -> Attendance ()
markInactive uid ts = do
    modifyTimeSheet $ MarkInactive uid ts
    sendIM uid "You will be marked as inactive from tomorrow onwards. Have a nice holiday!"

markActive :: UserId -> UTCTime -> Attendance ()
markActive uid ts = do
    modifyTimeSheet $ MarkActive uid ts
    sendIM uid "Welcome back! You have been marked as active from today onwards."

modifyTimeSheet :: TimeSheetUpdate -> Attendance ()
modifyTimeSheet ev = liftIO . flip logEvent ev . stateH =<< getAttnH

getTimeSheet :: Attendance TimeSheet
getTimeSheet = liftIO . getCurState . stateH =<< getAttnH

-------------------------------------------------------------------------------
-- UserTracker

trackUser :: IM -> Attendance ()
trackUser im = getAttnH >>= \h -> liftIO $ UT.trackUser (trackerH h) im

getTrackedUsers :: Attendance [UserId]
getTrackedUsers = getAttnH >>= \h -> liftIO $ UT.getTrackedUsers (trackerH h)

channelIsIM :: ChannelId -> Attendance Bool
channelIsIM cid = getAttnH >>= \h -> liftIO $ UT.channelIsIM (trackerH h) cid

-------------------------------------------------------------------------------
-- Slack helpers

sendIM :: UserId -> T.Text -> Attendance ()
sendIM uid msg = do
    h <- trackerH <$> getAttnH
    liftIO (UT.lookupIMChannel h uid) >>= \case
        Just cid -> sendMessage cid msg
        Nothing -> liftIO $ putStrLn $ "Couldn't find an IM channel for " ++ show uid
