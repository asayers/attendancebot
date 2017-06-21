{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import AtnBot.Actions
import AtnBot.Config
import AtnBot.Monad
import AtnBot.Report
import AtnBot.Schedule
import Attendance.BotState
import Attendance.TimeSheet
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Except
import qualified Data.Text as T
import Data.Thyme
import Data.Thyme.Clock.POSIX
import Data.Thyme.Time
import Web.Slack hiding (lines)

-------------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn $ "Writing data to " ++ logPath
    runAtnBot slackConfig logPath $ do
        liftIO $ putStrLn "Established slack connection"
        -- start cron thread
        liftIO $ putStrLn "Starting job scheduler..."
        either throwM runBotSchedule scheduledJobs
        -- run main loop
        liftIO $ putStrLn "Fetching spreadsheet data..."
        updateFromSpreadsheet
        liftIO $ putStrLn "Listening to events..."
        forever (nextEvent >>= handleEvent)

handleEvent :: Event -> AtnBot ()
handleEvent ev = case ev of
    ReactionAdded uid _ item_uid _ ts | item_uid == botUser ->
        checkin uid (timestampToUTCTime ts)
    Message cid (UserComment uid) msg (timestampToUTCTime -> ts) _ _ -> do
        isIM <- channelIsIM cid
        when (isIM && uid /= botUser) $ case msg of
            "debug" -> dumpDebug uid =<< either throwM return scheduledJobs
            "summary" -> sendRichIM uid "" . (:[]) =<< weeklySummary
            "spreadsheet" -> sendIM uid =<< printSpreadsheet
            "announce weekly" -> sendWeeklySummary
            "announce daily" -> sendDailySummary
            "remind missing" -> remindMissing
            _ -> checkin uid ts
    ImCreated _ im -> trackUser im
    PresenceChange _ _    -> return ()  -- expected, ignore
    ReconnectUrl _        -> return ()  -- expected, ignore
    UserTyping _ _        -> return ()  -- expected, ignore
    MessageResponse _ _ _ -> return ()  -- expected, ignore
    _ -> liftIO $ print ev              -- anything else is unexpected, log it

dumpDebug :: UserId -> BotSchedule -> AtnBot ()
dumpDebug uid sched = do
    ts <- flip userTimeSheet uid <$> getBotState
    -- curTime <- liftIO $ getCurrentTime
    -- session <- getSession
    -- let getUsername' target = maybe "unknown" _userName $
    --       find (\user -> _userId user == target) (_slackUsers session)
    sendIM uid $ T.concat
        [ "```\n"
        , ppTimesheet ts
        , ppSchedule sched
        , "```"
        ]

-------------------------------------------------------------------------------
-- Helpers

timestampToUTCTime :: SlackTimeStamp -> UTCTime
timestampToUTCTime = view (slackTime . getTime . thyme . from posixTime)
