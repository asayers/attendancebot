{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Attendance.Monad
    ( AttnH, withAttnH
    , Attendance, runAttendance
    , Scopes

      -- * Modifying
    , checkin
    , markInactive
    , markActive
    , markHoliday
    , trackUser

      -- * Querying
    , getTimeSheet
    , getTrackedUsers
    , channelIsIM

      -- * Google
    , uploadFile

      -- * Slack
    , sendIM
    , sendRichIM
    , getUsername
    , dumpDebug
    ) where

import Attendance.Log
import Attendance.Schedule
import Attendance.TimeSheet
import Attendance.UserTracker (TrackerHandle, newTrackerHandle)
import qualified Attendance.UserTracker as UT
import Control.Lens
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Data.Thyme
import Data.Time.Zones
import qualified Network.Google as G
import qualified Network.Google.Storage as G
import System.IO
import qualified Web.Slack.Handle as H
import Web.Slack.Monad

newtype Attendance a = Attendance (ReaderT AttnH (ResourceT IO) a)
    deriving ( Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch
             , MonadResource, MonadBase IO, MonadMask)

instance MonadBaseControl IO Attendance where
    type StM Attendance a = a
    liftBaseWith f = Attendance $ liftBaseWith $ \run -> f (\(Attendance x) -> run x)
    restoreM = Attendance . restoreM

instance MonadSlack Attendance where
    askSlackHandle = Attendance $ slackH <$> ask

instance G.MonadGoogle Scopes Attendance where
    liftGoogle (G.Google x) =
        liftResourceT . runReaderT x . googleEnv =<< getAttnH

data AttnH = AttnH
    { slackH :: H.SlackHandle
    , trackerH :: TrackerHandle
    , stateH :: LogHandle TimeSheetUpdate TimeSheet
    , googleEnv :: G.Env Scopes
    }

type Scopes = '[ "https://www.googleapis.com/auth/devstorage.read_write"
               , "https://www.googleapis.com/auth/spreadsheets.readonly"
               ]

withAttnH :: SlackConfig -> FilePath -> [UserId] -> TZ -> TimeOfDay -> (AttnH -> IO a) -> IO a
withAttnH conf logPath blacklist tz deadline fn = do
    let timeSheet = newTimeSheet tz deadline
    stateH <- newLogHandle updateTimeSheet timeSheetUpdate timeSheet logPath
    logger <- G.newLogger G.Debug stdout
    googleEnv <- (G.envLogger .~ logger) <$> G.newEnv
    H.withSlackHandle conf $ \slackH -> do
        trackerH <- newTrackerHandle (H.getSession slackH) blacklist
        fn AttnH{..}

runAttendance :: AttnH -> Attendance a -> IO a
runAttendance attnH (Attendance ma) =
    runResourceT $ runReaderT ma attnH

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

markHoliday :: UserId -> Day -> Double -> Attendance ()
markHoliday uid day amt = do
    modifyTimeSheet $ MarkHoliday uid day amt
    sendIM uid $ "Looks like you're taking the day off on " <> T.pack (show day) <> ". Have a nice time!"

modifyTimeSheet :: TimeSheetUpdate -> Attendance ()
modifyTimeSheet ev = liftIO . flip logEvent ev . stateH =<< getAttnH

getTimeSheet :: Attendance TimeSheet
getTimeSheet = liftIO . getCurState . stateH =<< getAttnH

dumpDebug :: UserId -> [CronJob (T.Text, a)] -> Attendance ()
dumpDebug uid scheduledJobs = do
    ts <- getTimeSheet
    curTime <- liftIO $ getCurrentTime
    session <- getSession
    let getUsername' target = maybe "unknown" _userName $
            find (\user -> _userId user == target) (_slackUsers session)
    sendIM uid $ T.unlines
        [ "```"
        , prettyPrintTimesheet ts curTime getUsername'
        , prettyPrintJobs fst scheduledJobs
        , "```"
        ]

-------------------------------------------------------------------------------
-- UserTracker

trackUser :: IM -> Attendance ()
trackUser im = getAttnH >>= \h -> liftIO $ UT.trackUser (trackerH h) im

getTrackedUsers :: Attendance [UserId]
getTrackedUsers = getAttnH >>= \h -> liftIO $ UT.getTrackedUsers (trackerH h)

channelIsIM :: ChannelId -> Attendance Bool
channelIsIM cid = getAttnH >>= \h -> liftIO $ UT.channelIsIM (trackerH h) cid

-------------------------------------------------------------------------------
-- Google

uploadFile :: G.MonadGoogle Scopes m => FilePath -> T.Text -> m T.Text
uploadFile filepath name = do
    let bucket = "attendancebot-141720.appspot.com"
    fileBody <- G.sourceBody filepath
    obj <- G.upload (G.objectsInsert bucket G.object' & (G.oiName .~ Just name) . (G.oiPredefinedACL .~ Just G.OIPAPublicRead)) fileBody
    return $ fromMaybe (error "uploadFile: no link returned") $ obj ^. G.objMediaLink

-------------------------------------------------------------------------------
-- Slack helpers

sendIM :: UserId -> T.Text -> Attendance ()
sendIM uid msg = do
    h <- trackerH <$> getAttnH
    liftIO (UT.lookupIMChannel h uid) >>= \case
        Just cid -> sendMessage cid msg
        Nothing -> liftIO $ putStrLn $ "Couldn't find an IM channel for " ++ show uid

sendRichIM :: UserId -> T.Text -> [Attachment] -> Attendance ()
sendRichIM uid msg attnts = do
    h <- trackerH <$> getAttnH
    liftIO (UT.lookupIMChannel h uid) >>= \case
        Just cid -> do
            ret <- sendRichMessage cid msg attnts
            either (liftIO . putStrLn . T.unpack) return ret
        Nothing -> liftIO $ putStrLn $ "Couldn't find an IM channel for " ++ show uid

-- | The user must already exist when the connection to slack is
-- established. TODO: get info from UserTracker
getUsername :: MonadSlack m => UserId -> m T.Text
getUsername uid =
    maybe "unknown" _userName .
        find (\user -> _userId user == uid) .
            _slackUsers <$> getSession
