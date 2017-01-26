{-# LANGUAGE OverloadedStrings #-}

module Attendance.Config
    ( getSlackConfig
    , getCheckinLog
    , getSpreadsheetId
    , getSpreadsheetRange
    , getBotUser
    , getAnnouncementChannel
    , blacklist
    , timezone
    , deadline
    ) where

import Data.Maybe
import qualified Data.Text as T
import Data.Thyme
import Data.Time.Zones
import Data.Time.Zones.All
import System.Environment
import Web.Slack hiding (lines)

-------------------------------------------------------------------------------
-- Configuration

getSlackConfig :: IO SlackConfig
getSlackConfig = SlackConfig <$> lookupEnvReq "SLACK_API_TOKEN"

getCheckinLog :: IO FilePath
getCheckinLog = lookupEnvReq "ATTENDANCE_LOG"

getSpreadsheetId :: IO T.Text
getSpreadsheetId = T.pack <$> lookupEnvReq "SPREADSHEET_ID"

getSpreadsheetRange :: IO T.Text
getSpreadsheetRange = T.pack <$> lookupEnvReq "SPREADSHEET_RANGE"

-- TODO: Get this from session
getBotUser :: IO UserId
getBotUser = Id . T.pack <$> lookupEnvReq "ATTENDANCEBOT_USER"

getAnnouncementChannel :: IO ChannelId
getAnnouncementChannel = Id . T.pack <$> lookupEnvReq "ANNOUCEMENT_CHANNEL"

-- | Users which we want to ignore
blacklist :: [UserId]
blacklist =
    [ Id "USLACKBOT" -- @slackbot
    ]

timezone :: TZ
timezone = tzByLabel Asia__Tokyo

deadline :: TimeOfDay
deadline = TimeOfDay 8 45 (fromSeconds' 0)  -- 8:45 am JST

-------------------------------------------------------------------------------

lookupEnvReq :: String -> IO String
lookupEnvReq name = fromMaybe (error $ name ++ " not set") <$> lookupEnv name

