{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module AtnBot.Report
    ( dailySummary
    , weeklySummary
    , missingReport
    ) where

import AtnBot.Monad
import Attendance.BotState
import Attendance.Timing
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
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Easy
import System.FilePath
import System.IO.Temp
import System.Locale
import Text.Printf
import Web.Slack hiding (lines)

missingReport :: AtnBot [(UserId, T.Text)]
missingReport = do
    missing <- lateComers <$> getBotState <*> liftIO today
    forM missing $ \uid -> do
        uname <- getUsername uid
        liftIO $ T.putStrLn $ "Sending reminder to " <> uname
        return (uid, "You haven't checked in yet today. Please send me a message or add a reaction to something I've said to let me know you're here.")

dailySummary :: AtnBot T.Text
dailySummary = do
    bs <- getBotState
    missing   <- fmap sort . mapM getUsername =<< lateComers    bs <$> liftIO today
    onHoliday <- fmap sort . mapM getUsername =<< holidayMakers bs <$> liftIO today
    streak <- goodRunLength bs <$> liftIO yesterday
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
    -- holiday uid =

-------------------------------------------------------------------------------
-- Weekly summary

weeklySummary :: AtnBot Attachment
weeklySummary = do
    users <- getTrackedUsers
    rows <- concat <$> mapM summaryRow users
    monday <- liftIO getMonday
    let monday' = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" monday
    -- let legend = T.unlines
    --         [ "◆  checked in before 9:00"
    --         , "◈  checked in after 9:00"
    --         , "◇  didn't check in"
    --         , "H  on holiday"
    --         , "The ratio of on-time to late is also shown"
    --         ]
    let legend = "Key: daily attendance | on-time:late ratio | score weighted by recency"
    colour <- getColour
    chartUrl <- renderWeeklySummaryChart
    return $ defaultAttachment
          { attachmentFields = rows
          , attachmentFooter = Just $ "Summary for the week beginning " <> monday' <> ".\n" <> legend
          , attachmentThumbUrl = Just chartUrl
          , attachmentFallback = "Attendance summary for the week beginning" <> monday'
          , attachmentColor = colour
          }

getColour :: AtnBot AttachmentColor
getColour = do
    bs <- getBotState
    users <- getTrackedUsers
    thisWeek <- liftIO getThisWeek
    let timings' = [ getTiming (userTimeSheet bs uid) day | uid <- users, day <- thisWeek ]
    let mkScore = \case OnTime{} -> 2; Late{} -> 0; Absent{} -> 0; OnHoliday{} -> 2
    let finalScore = sum (map mkScore timings') / fromIntegral (length timings')
    return $ case round (finalScore :: Double) :: Int of
        0 -> DangerColor
        1 -> WarningColor
        2 -> GoodColor
        _ -> DefaultColor

summaryRow :: UserId -> AtnBot [Field]
summaryRow uid = do
    bs <- getBotState
    uname <- getUsername uid
    badges <- map (badge . getTiming (userTimeSheet bs uid)) <$> liftIO getThisWeek
    td <- liftIO today
    let s = score (userTimeSheet bs uid) td
    let r = ratio (userTimeSheet bs uid) td
    let summary = T.pack $ printf "%s  |  %.1f:1  |  %.1f%%" badges r (s * 100)
    return [Field Nothing uname True, Field Nothing summary True]
  where
    badge OnTime{}    = '◆' -- '▩'   -- '◾' -- '◼' -- '■'
    badge Late{}      = '◈'  -- '◪' -- '▨'
    badge Absent{}    = '◇' -- '□'  -- '◽' -- '◻'-- '□'
    badge OnHoliday{} = 'H' -- '□'  -- '◽' -- '◻'-- '□'

-------------------------------------------------------------------------------
-- Weekly summary chart

renderWeeklySummaryChart :: AtnBot T.Text
renderWeeklySummaryChart =
    withSystemTempDirectory "weekly-attendance-graph" $ \outdir -> do
        timestamp <- formatTime defaultTimeLocale "%Y-%m-%d_%H-%M-%S" <$> liftIO getCurrentTime
        let outpath = outdir </> "out.png"
        let name = "weekly-attendance-graphs/" <> T.pack timestamp <> ".png"
        timeSheet <- getBotState
        liftIO $ makeChart timeSheet outpath
        uploadFile outpath name

makeChart :: BotState -> FilePath -> IO ()
makeChart bs outpath = do
    recentDays <- take 5 . pastWeekDays <$> today
    let dataset = map (summaryOfDay bs) recentDays
    let chart = def
          -- & plot_bars_titles .~ ["On time","Late"]
          & plot_bars_style .~ BarsStacked
          & plot_bars_values .~ dataset
          & plot_bars_spacing .~ BarsFixWidth 10
    let layout = def & layout_plots .~ [ plotBars chart ]
    let fileOpts = FileOptions{ _fo_size = (125,125), _fo_format = PNG }
    void $ renderableToFile fileOpts outpath (layoutToRenderable layout)

summaryOfDay :: BotState -> Day -> (PlotIndex, [Int])
summaryOfDay bs day =
    (PlotIndex (fromEnum day), [onTime, late])
  where
    onTime = length $ filter (isOnTime . snd) $ timings bs day
    late   = length $ filter (isLate   . snd) $ timings bs day

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

listEN :: [T.Text] -> T.Text
listEN [] = ""
listEN [x] = x
listEN [x,y] = x <> " and " <> y
listEN (x:xs) = x <> ", " <> listEN xs

pl :: Int -> T.Text -> T.Text -> T.Text
pl x sing plural = if x == 1 then sing else plural
