{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module AtnBot.Config
    ( Scopes
    , slackConfig
    , logPath
    , spreadsheetId
    , spreadsheetRange
    , botUser
    , announcementChannel
    , uploadBucket
    , blacklist
    , timezone
    , deadline
    ) where

import Data.Maybe
import qualified Data.Text as T
import Data.Thyme
import Data.Time.Zones
import System.Environment
import System.IO.Unsafe
import Web.Slack hiding (lines)

type Scopes = '[ "https://www.googleapis.com/auth/devstorage.read_write"
               , "https://www.googleapis.com/auth/spreadsheets.readonly"
               ]

slackConfig :: SlackConfig
slackConfig = SlackConfig $ lookupEnvReq "SLACK_API_TOKEN"

logPath :: FilePath
logPath = lookupEnvReq "ATTENDANCE_LOG"

spreadsheetId :: T.Text
spreadsheetId = T.pack $ lookupEnvReq "SPREADSHEET_ID"

spreadsheetRange :: T.Text
spreadsheetRange = T.pack $ lookupEnvReq "SPREADSHEET_RANGE"

-- TODO: Get this from session
botUser :: UserId
botUser = Id . T.pack $ lookupEnvReq "ATTENDANCEBOT_USER"

announcementChannel :: ChannelId
announcementChannel = Id . T.pack $ lookupEnvReq "ANNOUNCEMENT_CHANNEL"
-- announcementChannel = Id "C02FJ64LD"  -- #hr-general

uploadBucket :: T.Text
uploadBucket = T.pack $ lookupEnvReq "UPLOAD_BUCKET"

-- | Users which we want to ignore
blacklist :: [UserId]
blacklist =
    [ Id "USLACKBOT" -- @slackbot
    ]

timezone :: TZ
timezone = unsafePerformIO loadLocalTZ
{-# NOINLINE timezone #-}

deadline :: TimeOfDay
deadline = TimeOfDay 8 45 (fromSeconds' 0)  -- 8:45 am JST

-------------------------------------------------------------------------------

lookupEnvReq :: String -> String
lookupEnvReq name = unsafePerformIO $   -- I feel OK about this...
    fromMaybe (error $ name ++ " not set") <$> lookupEnv name
{-# NOINLINE lookupEnvReq #-}
