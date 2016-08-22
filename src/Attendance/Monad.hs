{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Attendance.Monad
    ( AttnH, withAttnH
    , Attendance, runAttendance

      -- * Primitive Attendance monad actions
    , getTrackedUsers
    , lookupIMChannel
    , channelIsIM
    , getTimeSheet
    , addCheckIn
    , addTrackedUser
    , logCheckIn
    , getCheckInHistory
    ) where

import Attendance.CheckIn
import Attendance.TimeSheet
import Control.Lens
import Control.Monad.Reader
import qualified Data.HashMap.Strict as HMS
import Data.IORef
import Web.Slack.Handle (SlackHandle, withSlackHandle)
import Web.Slack.Monad

newtype Attendance a = Attendance (ReaderT AttnH IO a)
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadSlack Attendance where
    askSlackHandle = Attendance $ slackH <$> ask

data AttnH = AttnH
    { slackH :: SlackHandle
    , attnS :: IORef AttnS
    }

data AttnS = AttnS
    { _trackedUsers :: HMS.HashMap UserId ChannelId
    , _timeSheet :: TimeSheet
    , _checkinLog :: FilePath
    }

makeLenses ''AttnS

withAttnH :: SlackConfig -> FilePath -> (AttnH -> IO a) -> IO a
withAttnH conf _checkinLog fn = withSlackHandle conf $ \slackH -> do
    _timeSheet <- newTimeSheet
    let _trackedUsers = HMS.empty
    attnS <- newIORef AttnS{..}
    fn AttnH{..}

runAttendance :: AttnH -> Attendance a -> IO a
runAttendance attnH (Attendance ma) = do
    runReaderT ma attnH

readAttnS :: Attendance AttnS
modifyAttnS :: (AttnS -> AttnS) -> Attendance ()
readAttnS      = Attendance $ liftIO . readIORef           . attnS =<< ask
modifyAttnS fn = Attendance $ liftIO . flip modifyIORef fn . attnS =<< ask

-------------------------------------------------------------------------------
-- Primitive Attendance monad actions

getTrackedUsers :: Attendance [UserId]
getTrackedUsers = HMS.keys . view trackedUsers <$> readAttnS

lookupIMChannel :: UserId -> Attendance (Maybe ChannelId)
lookupIMChannel uid = HMS.lookup uid . view trackedUsers <$> readAttnS

channelIsIM :: ChannelId -> Attendance Bool
channelIsIM cid = (cid `elem`) . HMS.elems . view trackedUsers <$> readAttnS

getTimeSheet :: Attendance TimeSheet
getTimeSheet = view timeSheet <$> readAttnS

addCheckIn :: CheckIn -> Attendance ()
addCheckIn ci = modifyAttnS $ over timeSheet (updateTimeSheet ci)

addTrackedUser :: UserId -> ChannelId -> Attendance ()
addTrackedUser uid cid = modifyAttnS $ over trackedUsers (HMS.insert uid cid)

logCheckIn :: CheckIn -> Attendance ()
logCheckIn ci = do
    AttnS{..} <- readAttnS
    liftIO $ writeCheckIn _checkinLog ci

getCheckInHistory :: Attendance [CheckIn]
getCheckInHistory =
    liftIO . readCheckIns . view checkinLog =<< readAttnS
