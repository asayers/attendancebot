{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

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
import Data.AffineSpace
import Data.List (find, sort)
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Thyme
import Data.Thyme.Calendar.WeekDate
import Data.Thyme.Clock.POSIX
import Data.Thyme.Time
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Easy
import System.FilePath
import System.IO.Temp
import System.Locale
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
    ts <- getTimeSheet
    missing   <- fmap sort . mapM getUserName =<< lateComers    ts <$> liftIO today
    onHoliday <- fmap sort . mapM getUserName =<< holidayMakers ts <$> liftIO today
    streak <- goodRunLength ts <$> liftIO yesterday
    let missingTxt = case missing of
          [] -> "Everyone got in on time! Current streak: " <>
                T.pack (show $ streak + 1) <> pl (streak + 1) " day." " days."
          _ | streak > 0 ->
                   "C-c-c-combo breaker! " <> listEN missing <> " still " <>
                   pl (length missing) "hasn't" "haven't" <> " checked in. " <>
                   "This ends a streak of " <> T.pack (show streak) <>
                   pl streak " day." " days."
          _ -> listEN missing <> " still " <> pl (length missing) "hasn't" "haven't" <> " checked in."
    let holidayTxt = case onHoliday of
          [] -> ""
          _ -> " " <> listEN onHoliday <> pl (length onHoliday) " is" " are" <> " on holiday today."
    return $ missingTxt <> holidayTxt

-------------------------------------------------------------------------------
-- Weekly summary

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

-------------------------------------------------------------------------------
-- Weekly summary chart

renderWeeklySummaryChart :: Attendance T.Text
renderWeeklySummaryChart =
    withSystemTempDirectory "weekly-attendance-graph" $ \outdir -> do
        timestamp <- formatTime defaultTimeLocale "%Y-%m-%d_%H-%M-%S" <$> liftIO getCurrentTime
        let outpath = outdir </> "out.png"
        let name = "weekly-attendance-graphs/" <> T.pack timestamp <> ".png"
        timeSheet <- getTimeSheet
        liftIO $ makeChart timeSheet outpath
        uploadFile outpath name

makeChart :: TimeSheet -> FilePath -> IO ()
makeChart ts outpath = do
    recentDays <- take 20 . pastWeekDays <$> today
    let dataset = map (summaryOfDay ts) recentDays
    let chart = def
          & plot_bars_titles .~ ["On time","Late"]
          & plot_bars_style .~ BarsStacked
          & plot_bars_values .~ dataset
    let layout = def & layout_plots .~ [ plotBars chart ]
    let fileOpts = FileOptions{ _fo_size = (400,150), _fo_format = PNG }
    void $ renderableToFile fileOpts outpath (layoutToRenderable layout)

summaryOfDay :: TimeSheet -> Day -> (PlotIndex, [Int])
summaryOfDay ts day =
    (PlotIndex (fromEnum day), [onTime, late])
  where
    checkins = map (\uid -> lookupTiming ts uid day) (allUsers ts)
    onTime = length $ filter isOnTime checkins
    late   = length $ filter isLate   checkins

isOnTime, isLate :: Timing -> Bool
isOnTime = \case OnTime _ -> True; _ -> False
isLate   = \case Late   _ -> True; _ -> False
-- isAbsent = \case Absent   -> True; _ -> False

-------------------------------------------------------------------------------
-- Time helpers

today :: IO Day
today = localDay . zonedTimeToLocalTime <$> getZonedTime

yesterday :: IO Day
yesterday = head . drop 1 . pastWeekDays <$> today

-- | An infinite list of weekdays, in reverse order, starting from the
-- given day and working backwards (inclusive).
pastWeekDays :: Day -> [Day]
pastWeekDays d = map (d .-^ ) [0..]   -- FIXME

-- | The weekdays (excluding the weekend) of the current week.
getThisWeek :: IO [Day]
getThisWeek = do
    x <- view weekDate <$> today
    return $ map (\day -> review weekDate x{ wdDay = day }) [1..5]

-- getThisMonth :: IO [Day]
-- getThisMonth = fmap (view weekDate) today <&> \WeekDate{..} ->
--     [ review weekDate (WeekDate wdYear w d)
--     | w <- map (\i -> wdWeek - i) [0,1,2], d <- [1..5]
--     ]

-- | The monday of the current week
getMonday :: IO Day
getMonday = do
    x <- view weekDate <$> today
    return $ review weekDate x{ wdDay = 1 }

-------------------------------------------------------------------------------

-- | The user must already exist when the connection to slack is established.
getUserName :: MonadSlack m => UserId -> m T.Text
getUserName uid =
    maybe "unknown" _userName .
    find (\user -> _userId user == uid) .
    _slackUsers <$> getSession

-------------------------------------------------------------------------------

listEN :: [T.Text] -> T.Text
listEN [] = ""
listEN [x] = x
listEN [x,y] = x <> " and " <> y
listEN (x:xs) = x <> ", " <> listEN xs

pl :: Int -> T.Text -> T.Text -> T.Text
pl x sing plural = if x == 1 then sing else plural
