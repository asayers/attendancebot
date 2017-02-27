{-# LANGUAGE OverloadedStrings #-}

module AtnBot.Actions
    ( checkin
    , sendWeeklySummary
    , sendDailySummary
    , remindMissing
    , updateFromSpreadsheet
    , printSpreadsheet
    ) where

import AtnBot.Config
import AtnBot.Monad
import AtnBot.Report
import AtnBot.Spreadsheet
import Attendance.BotState
import Attendance.TimeSheet
import Attendance.Timing
import Control.Monad.Except
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
import Data.Thyme
import Web.Slack hiding (lines)

-------------------------------------------------------------------------------
-- TimeSheet

checkin :: UserId -> UTCTime -> AtnBot ()
checkin uid ts = do
    modifyBotState $ CheckIn uid ts
    sendIM uid "Your attendance has been noted. Have a good day!"

-------------------------------------------------------------------------------
-- Reports

sendDailySummary :: AtnBot ()
sendDailySummary = sendMessage announcementChannel =<< dailySummary

remindMissing :: AtnBot ()
remindMissing = mapM_ (uncurry sendIM) =<< missingReport

sendWeeklySummary :: AtnBot ()
sendWeeklySummary = do
    attachment <- weeklySummary
    ret <- sendRichMessage announcementChannel "" [attachment]
    either (liftIO . putStrLn . T.unpack) return ret

-------------------------------------------------------------------------------
-- Spreadsheet

updateFromSpreadsheet :: AtnBot ()
updateFromSpreadsheet = do
    xs <- getAttendanceData
    bs <- getBotState
    forM_ (HMS.toList xs) $ \((uid, day), amt) ->
        case getTiming (userTimeSheet bs uid) day of
            _ | amt <= 0 -> return ()
            OnHoliday -> return ()
            _ -> modifyBotState $ MarkHoliday uid day amt
    -- sendIM uid $ "Looks like you're taking the day off on " <> T.pack (show day) <> ". Have a nice time!"

printSpreadsheet :: AtnBot T.Text
printSpreadsheet = do
    spreadsheet <- getAttendanceData
    usernames <- getUsernames
    return $ ppSpreadsheet usernames spreadsheet
