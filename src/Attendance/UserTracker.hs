{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Attendance.UserTracker
    ( TrackerHandle
    , newTrackerHandle
    , trackUser
    , getTrackedUsers
    , lookupIMChannel
    , channelIsIM
    ) where

import Control.Lens
import Control.Monad
import qualified Data.HashMap.Strict as HMS
import Data.IORef
import Data.Monoid
import qualified Data.Text.IO as T
import Web.Slack hiding (lines)

newtype TrackerHandle = TrackerHandle (IORef Handle')
data Handle' = Handle'
    { _trackedUsers :: HMS.HashMap UserId ChannelId
    , _blacklist :: [UserId]
    }

makeLenses ''Handle'

newTrackerHandle :: SlackSession -> [UserId] -> IO TrackerHandle
newTrackerHandle session _blacklist = do
    let _trackedUsers = HMS.empty
    h <- TrackerHandle <$> newIORef Handle'{..}
    traverseOf_ (slackIms . traverse) (trackUser h) session
    return h

trackUser :: TrackerHandle -> IM -> IO ()
trackUser (TrackerHandle h) im = do
    let uid = im ^. imUser
    let cid = im ^. imId . to imToChannel
    shouldIgnore <- elem uid . view blacklist <$> readIORef h
    unless shouldIgnore $ do
        T.putStrLn $ "Tracking user " <> _getId uid
        modifyIORef h $ set (trackedUsers . at uid) (Just cid)

getTrackedUsers :: TrackerHandle -> IO [UserId]
getTrackedUsers (TrackerHandle h) = HMS.keys . view trackedUsers <$> readIORef h

lookupIMChannel :: TrackerHandle -> UserId -> IO (Maybe ChannelId)
lookupIMChannel (TrackerHandle h) uid = preview (trackedUsers . ix uid) <$> readIORef h

channelIsIM :: TrackerHandle -> ChannelId -> IO Bool
channelIsIM (TrackerHandle h) cid = elemOf (trackedUsers . traverse) cid <$> readIORef h
