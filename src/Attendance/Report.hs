{-# LANGUAGE OverloadedStrings #-}

module Attendance.Report
    ( dailySummary
    , weeklySummary
    , missingReport
    ) where

import Attendance.Monad
import Attendance.TimeSheet
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Data.List (find, sort)
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Thyme
import Data.Thyme.Clock.POSIX
import Data.Thyme.Time
import System.Locale
import System.Process
import Web.Slack hiding (lines)

missingReport :: Attendance [(UserId, T.Text)]
missingReport = do
    missing <- lateComers <$> getTimeSheet <*> liftIO today
    forM missing $ \uid -> do
        uname <- getUserName uid
        liftIO $ T.putStrLn $ "Sending reminder to " <> uname
        return (uid, "You haven't checked in yet today. Please send me a message or add a reaction to something I've said to let me know you're here.")

dailySummary :: Attendance T.Text
dailySummary = do
    missing <- fmap sort . mapM getUserName =<< lateComers <$> getTimeSheet <*> liftIO today
    streak <- goodRunLength <$> getTimeSheet <*> liftIO yesterday
    return $ case missing of
        [] -> "Everyone got in on time! Current streak: " <>
              T.pack (show $ streak + 1) <> pl (streak + 1) " day." " days."
        _ | streak > 0 ->
                 "C-c-c-combo breaker! " <> listEN missing <> " still " <>
                 pl (length missing) "hasn't" "haven't" <> " checked in. " <>
                 "This ends a streak of " <> T.pack (show streak) <>
                 pl streak " day." " days."
        _ -> listEN missing <> " still " <> pl (length missing) "hasn't" "haven't" <> " checked in."

weeklySummary :: Attendance Attachment
weeklySummary = do
    users <- getTrackedUsers
    rows <- concat <$> mapM summaryRow users
    monday <- liftIO getMonday
    let monday' = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" monday
    -- let legend = T.unlines
    --         [ "◆  checked in before 9:00"
    --         , "◈  checked in after 9:00"
    --         , "◇  didn't check in"
    --         ]
    let ts = UTCTime monday (fromSeconds' 0) ^. from utcTime . posixTime . from thyme
    chartUrl <- renderWeeklySummaryChart
    return $ defaultAttachment
          { attachmentTitle  = Just $ "Week beginning " <> monday'
          , attachmentFields = rows
          , attachmentFooter = Just "Weekly summary"
          , attachmentTs     = Just ts
          , attachmentImageUrl = Just chartUrl
          , attachmentFallback = "Attendance summary for the week beginning" <> monday'
          }

summaryRow :: UserId -> Attendance [Field]
summaryRow uid = do
    ts <- getTimeSheet
    uname <- getUserName uid
    badges <- T.pack . map (badge . lookupTiming ts uid) <$> liftIO getThisWeek
    return [Field Nothing uname True, Field Nothing badges True]
  where
    badge (OnTime _) = '◆' -- '▩'   -- '◾' -- '◼' -- '■'
    badge (Late   _) = '◈'  -- '◪' -- '▨'
    badge Absent     = '◇' -- '□'  -- '◽' -- '◻'-- '□'
    badge OnHoliday  = 'H' -- '□'  -- '◽' -- '◻'-- '□'

renderWeeklySummaryChart :: Attendance T.Text
renderWeeklySummaryChart = do
    timeSheet <- getTimeSheet
    timestamp <- formatTime defaultTimeLocale "%Y-%m-%d_%H-%M-%S" <$> liftIO getCurrentTime
    liftIO $ makeChart timeSheet "out.png"
    liftIO $ callCommand $ "scp out.png alex@www.asayers.org:/var/www/asayers.org/html/attendance/" ++ timestamp ++ ".png"
    return $ T.pack $ "http://www.asayers.org/attendance/" ++ timestamp ++ ".png"

-- | The user must already exist when the connection to slack is established.
getUserName :: MonadSlack m => UserId -> m T.Text
getUserName uid =
    maybe "unknown" _userName .
    find (\user -> _userId user == uid) .
    _slackUsers <$> getSession

listEN :: [T.Text] -> T.Text
listEN [] = ""
listEN [x] = x
listEN [x,y] = x <> " and " <> y
listEN (x:xs) = x <> ", " <> listEN xs

pl :: Int -> T.Text -> T.Text -> T.Text
pl x sing plural = if x == 1 then sing else plural
