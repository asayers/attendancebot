{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module AtnBot.UserTracker
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

data TrackerHandle = TrackerHandle
    { trackedUsers :: IORef (HMS.HashMap UserId ChannelId)
    , blacklist :: [UserId]
    }

newTrackerHandle :: SlackSession -> [UserId] -> IO TrackerHandle
newTrackerHandle session blacklist = do
    trackedUsers <- newIORef HMS.empty
    let h = TrackerHandle{..}
    traverseOf_ (slackIms . traverse) (trackUser h) session
    return h

trackUser :: TrackerHandle -> IM -> IO ()
trackUser TrackerHandle{..} im = do
    let uid = im ^. imUser
    let cid = im ^. imId . to imToChannel
    unless (elem uid blacklist) $ do
        T.putStrLn $ "Tracking user " <> _getId uid
        modifyIORef trackedUsers $ HMS.insert uid cid

getTrackedUsers :: TrackerHandle -> IO [UserId]
getTrackedUsers TrackerHandle{..} = HMS.keys <$> readIORef trackedUsers

lookupIMChannel :: TrackerHandle -> UserId -> IO (Maybe ChannelId)
lookupIMChannel TrackerHandle{..} uid = HMS.lookup uid <$> readIORef trackedUsers

channelIsIM :: TrackerHandle -> ChannelId -> IO Bool
channelIsIM TrackerHandle{..} cid = elem cid <$> readIORef trackedUsers
