{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Attendance.CheckIn
import Attendance.Monad
import Attendance.TimeSheet
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Data.Default.Class
import Data.List (find, sort)
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Thyme
import Data.Thyme.Clock.POSIX
import Data.Thyme.Time
import System.Cron.Schedule
import System.Environment
import System.Locale
import System.Process
import Web.Slack hiding (lines)

-- TODO: weekends

-------------------------------------------------------------------------------

main :: IO ()
main = do
    checkinLog <- getCheckinLog
    withAttnH slackConfig checkinLog $ \h -> do
        -- initialise state
        runAttendance h $ do
            traverseOf_ (slackIms . traverse) trackUser =<< getSession
            mapM_ addCheckIn =<< getCheckInHistory
        -- start cron thread
        void $ execSchedule $ schedule h
        -- run main loop
        runAttendance h $ forever (getNextEvent >>= handleEvent)

handleEvent :: Event -> Attendance ()
handleEvent = \case
    ReactionAdded uid _ item_uid _ ts | item_uid == user_me ->
        checkin (CheckIn uid (timestampToUTCTime ts))
    Message cid (UserComment uid) _ ts _ _ -> do
        isIM <- channelIsIM cid
        when isIM $ checkin (CheckIn uid (timestampToUTCTime ts))
    ImCreated _ im -> trackUser im
    ev -> liftIO $ print ev

-------------------------------------------------------------------------------
-- Configuration

schedule :: AttnH -> Schedule ()
schedule h = do
    addJob (runAttendance h remindMissing) "45 23 * * 0-4"  -- 8:45 mon-fri
    addJob (runAttendance h dailySummary) "55 23 * * 0-4"  -- 8:55 mon-fri
    addJob (runAttendance h weeklySummary) "31 3 * * 5"    -- midday on friday

slackConfig :: SlackConfig
slackConfig = SlackConfig { _slackApiToken = "" }

getCheckinLog :: IO FilePath
getCheckinLog = do
    checkinLog <- fromMaybe (error "CHECKIN_LOG not set") <$> lookupEnv "CHECKIN_LOG"
    ensureExists checkinLog
    return checkinLog

-- | Users which we want to ignore
blacklist :: [UserId]
blacklist =
    [ Id "USLACKBOT" -- @slackbot
    ]

-- TODO: Get this from session
user_me :: UserId
user_me = Id ""  -- @attendancebot

channel_announce :: ChannelId
channel_announce = Id ""

channel_test :: ChannelId
channel_test = Id ""

-------------------------------------------------------------------------------
-- Bot actions

trackUser :: IM -> Attendance ()
trackUser im = do
    let uid = im ^. imUser
    uname <- getUserName uid
    if elem uid blacklist
      then liftIO $ T.putStrLn $ "Ignoring user " <> uname
      else do
          liftIO $ T.putStrLn $ "Tracking user " <> uname
          addTrackedUser (im ^. imUser) (im ^. imId . to imToChannel)

-- | Mark a user as checked-in
checkin :: CheckIn -> Attendance ()
checkin ci@CheckIn{..} =
    when (ciUser /= user_me) $ do
        addCheckIn ci
        logCheckIn ci
        sendIM ciUser "Your attendance has been noted. Have a good day!"

dailySummary :: Attendance ()
dailySummary = do
    missing <- fmap sort . mapM getUserName =<< lateComers <$> getTimeSheet <*> liftIO today
    streak <- goodRunLength <$> getTimeSheet <*> liftIO yesterday
    sendMessage channel_announce $ case missing of
      [] -> "Everyone got in on time! Current streak: " <>
            T.pack (show $ streak + 1) <> pl (streak + 1) " day." " days."
      _ | streak > 0 ->
               "C-c-c-combo breaker! " <> listEN missing <> " still " <>
               pl (length missing) "hasn't" "haven't" <> " checked in. " <>
               "This ends a streak of " <> T.pack (show streak) <>
               pl streak " day." " days."
      _ -> listEN missing <> " still " <> pl (length missing) "hasn't" "haven't" <> " checked in."

listEN :: [T.Text] -> T.Text
listEN [] = ""
listEN [x] = x
listEN [x,y] = x <> " and " <> y
listEN (x:xs) = x <> ", " <> listEN xs

pl :: Int -> T.Text -> T.Text -> T.Text
pl x sing plural = if x == 1 then sing else plural

remindMissing :: Attendance ()
remindMissing = do
    missing <- lateComers <$> getTimeSheet <*> liftIO today
    forM_ missing $ \uid -> do
        uname <- getUserName uid
        liftIO $ T.putStrLn $ "Sending reminder to " <> uname
        sendIM uid "You haven't checked in yet today. Please send me a message or add a reaction to something I've said to let me know you're here."

renderWeeklySummaryChart :: Attendance T.Text
renderWeeklySummaryChart = do
    timeSheet <- getTimeSheet
    timestamp <- formatTime defaultTimeLocale "%Y-%m-%d_%H-%M-%S" <$> liftIO getCurrentTime
    liftIO $ makeChart timeSheet "out.png"
    liftIO $ callCommand $ "scp out.png alex@www.asayers.org:/var/www/asayers.org/html/attendance/" ++ timestamp ++ ".png"
    return $ T.pack $ "http://www.asayers.org/attendance/" ++ timestamp ++ ".png"

weeklySummary :: Attendance ()
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
    let attachment = defaultAttachment
          { attachmentTitle  = Just $ "Week beginning " <> monday'
          , attachmentFields = rows
          , attachmentFooter = Just "Weekly summary"
          , attachmentTs     = Just ts
          , attachmentImageUrl = Just chartUrl
          , attachmentFallback = "Attendance summary for the week beginning" <> monday'
          }
    either (liftIO . putStrLn . T.unpack) return =<<
        sendRichMessage channel_announce "" [attachment]

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

-------------------------------------------------------------------------------
-- Helpers

-- instance FormatTime SlackTimeStamp where
--     formatCharacter c = (\f x y (SlackTimeStamp t _) -> f x y t) <$> formatCharacter c

timestampToUTCTime :: SlackTimeStamp -> UTCTime
timestampToUTCTime = view (slackTime . getTime . thyme . from posixTime)

-- forkSlack :: Slack () -> Slack ThreadId
-- forkSlack x =
--     liftIO . forkIO . runReaderT x =<< ask

-------------------------------------------------------------------------------
-- Slack helpers

-- | The user must already exist when the connection to slack is established.
getUserName :: MonadSlack m => UserId -> m T.Text
getUserName uid =
    maybe "unknown" _userName .
    find (\user -> _userId user == uid) .
    _slackUsers <$> getSession

sendIM :: UserId -> T.Text -> Attendance ()
sendIM uid msg =
    lookupIMChannel uid >>= \case
        Just cid -> sendMessage cid msg
        Nothing -> liftIO $ putStrLn $ "Couldn't find an IM channel for " ++ show uid
