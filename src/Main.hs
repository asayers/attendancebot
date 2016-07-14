{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Cron
import qualified Data.HashMap.Strict as HMS
import Data.IORef
import Data.List (find)
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format
import Web.Slack hiding (lines)

-- TODO: weekends

type BotH = IORef BotState
data BotState = BotState
    { _checkIns :: [CheckIn]
    , _trackedUsers :: HMS.HashMap UserId ChannelId
    }

type CheckIn = (UserId, UTCTime)

makeLenses ''BotState

initialBotState :: BotState
initialBotState = BotState
    { _checkIns = []
    , _trackedUsers = HMS.empty
    }

-------------------------------------------------------------------------------

main :: IO ()
main = runSlack config $ do
    -- initialise state
    st <- liftIO $ newIORef initialBotState
    traverseOf_ (slackIms . traverse) (trackUser st) =<< getSession
    traverseOf_ (lined . to T.pack . to parseCheckin . _Just) (checkinNoLog st)
        =<< liftIO (readFile checkinLog)
    -- start cron thread
    void $ forkSlack $ runCron (schedule st)
    -- run main loop
    forever $ getNextEvent >>= handleEvent st

handleEvent :: BotH -> Event -> Slack ()
handleEvent st r = case r of
    ReactionAdded uid _ item_uid _ ts | item_uid == user_me ->
        checkin st (uid, timestampToUTCTime ts)
    Message cid (UserComment uid) _ ts _ _ -> do
        isIM <- channelIsIM st cid
        when isIM $ checkin st (uid, timestampToUTCTime ts)
    ImCreated _ im -> trackUser st im
    _ -> liftIO $ print r

-------------------------------------------------------------------------------
-- Configuration

checkinLog :: FilePath
checkinLog = "checkins.log"

schedule :: BotH -> CronTab Slack
schedule st =
    [ CronJob (hrs 15 + mins 00) (clearCheckIns st)     -- midnight JST
    , CronJob (hrs 23 + mins 45) (remindMissing st)     -- 8:45 JST
    , CronJob (hrs 23 + mins 55) (reportMissing st)     -- 8:55 JST
    ]

config :: SlackConfig
config = SlackConfig { _slackApiToken = "" }

-- | Users which we want to ignore
blacklist :: [UserId]
blacklist = [user_slackbot]

-- TODO: Get this from session
user_me :: UserId
user_me = Id ""

user_slackbot :: UserId
user_slackbot = Id "USLACKBOT"

channel_announce :: ChannelId
channel_announce = Id ""

-------------------------------------------------------------------------------
-- Serialisation

printCheckin :: CheckIn -> Slack T.Text
printCheckin (Id uid, ts) = do
    uname <- getUserName (Id uid)
    return $ T.intercalate "\t"
        [ fmtTime ts
        , uid
        , uname
        ]

parseCheckin :: T.Text -> Maybe CheckIn
parseCheckin txt =
    case T.splitOn "\t" txt of
        (x:y:_) -> (,) (Id y) <$> prsTime x
        _ -> Nothing

fmtTime :: UTCTime -> T.Text
fmtTime = T.pack . formatTime defaultTimeLocale "%s"
prsTime :: T.Text -> Maybe UTCTime
prsTime = parseTimeM True defaultTimeLocale "%s" . T.unpack

-------------------------------------------------------------------------------
-- Accessing the state

getCheckIns :: MonadIO m => BotH -> m [CheckIn]
getCheckIns = liftIO . fmap _checkIns . readIORef

getTrackedUsers :: MonadIO m => BotH -> m [UserId]
getTrackedUsers = liftIO . fmap (HMS.keys . _trackedUsers) . readIORef

lookupIMChannel :: MonadIO m => BotH -> UserId -> m (Maybe ChannelId)
lookupIMChannel st uid =
    liftIO $ fmap (HMS.lookup uid . _trackedUsers) $ readIORef st

channelIsIM :: MonadIO m => BotH -> ChannelId -> m Bool
channelIsIM st cid =
    liftIO $ fmap ((cid `elem`) . HMS.elems . _trackedUsers) $ readIORef st

getMissingUsers :: MonadIO m => BotH -> m [UserId]
getMissingUsers st = do
    cis   <- getCheckIns st
    let notCheckedIn uid = null $ filter (\(x,_) -> x == uid) cis
    filter notCheckedIn <$> getTrackedUsers st

-- | The user must already exist when the connection to slack is established.
getUserName :: UserId -> Slack T.Text
getUserName uid =
    maybe "unknown" _userName .
    find (\user -> _userId user == uid) .
    _slackUsers <$> getSession

-------------------------------------------------------------------------------
-- Bot actions

trackUser :: BotH -> IM -> Slack ()
trackUser st im = do
    let uid = im ^. imUser
    uname <- getUserName uid
    liftIO $ if elem uid blacklist
      then T.putStrLn $ "Ignoring user " <> uname
      else do
          T.putStrLn $ "Tracking user " <> uname
          modifyIORef st $ over trackedUsers $
              HMS.insert (im ^. imUser) (im ^. imId . to imToChannel)

-- | Mark a user as checked-in
checkin :: BotH -> CheckIn -> Slack ()
checkin st ci@(uid,_) =
    when (uid /= user_me) $ do
        checkinNoLog st ci
        ciStr <- printCheckin ci
        liftIO $ T.appendFile checkinLog (ciStr <> "\n")
        sendIM st uid "Your attendance has been noted. Have a good day!"

checkinNoLog :: BotH -> CheckIn -> Slack ()
checkinNoLog st ci@(uid, ts) = do
    name <- getUserName uid
    liftIO $ T.putStrLn $ "Check-in: " <> name <> " at " <> ppTime ts
    liftIO $ modifyIORef st $ over checkIns (ci:)

clearCheckIns :: BotH -> Slack ()
clearCheckIns st =
    liftIO $ modifyIORef st $ set checkIns []

reportMissing :: BotH -> Slack ()
reportMissing st = do
    missing <- getMissingUsers st
    msg <- fmap T.unlines $ sequence $
        return "The following people still haven't checked in:" :
        map (fmap ("* " <>) . getUserName) missing
    sendMessage channel_announce msg

remindMissing :: BotH -> Slack ()
remindMissing st = do
    missing <- getMissingUsers st
    forM_ missing $ \uid ->
        sendIM st uid "You haven't checked in yet today. Please send me a message or add a reaction to something I've said to let me know you're here."

-------------------------------------------------------------------------------
-- Helpers

ppTime :: FormatTime a => a -> T.Text
ppTime = T.pack . formatTime defaultTimeLocale "%H:%M"

-- instance FormatTime SlackTimeStamp where
--     formatCharacter c = (\f x y (SlackTimeStamp t _) -> f x y t) <$> formatCharacter c

timestampToUTCTime :: SlackTimeStamp -> UTCTime
timestampToUTCTime = view (slackTime . getTime . to posixSecondsToUTCTime)

forkSlack :: Slack () -> Slack ThreadId
forkSlack x =
    liftIO . forkIO . runReaderT x =<< ask

sendIM :: BotH -> UserId -> T.Text -> Slack ()
sendIM st uid msg =
    lookupIMChannel st uid >>= \case
        Just cid -> sendMessage cid msg
        Nothing -> liftIO $ putStrLn $ "Couldn't find an IM channel for " ++ show uid
