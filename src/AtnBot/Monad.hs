{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module AtnBot.Monad
    ( AtnBot
    , runAtnBot

    , modifyBotState
    , getBotState
    , trackUser
    , getTrackedUsers
    , channelIsIM

      -- * Google
    , uploadFile

      -- * Slack
    , nextEvent
    , sendMsg
    , sendRichMsg
    , sendIM
    , sendRichIM
    , getUsername
    , getUsernames
    ) where

import AtnBot.Config
import AtnBot.DB
import AtnBot.UserTracker (TrackerHandle, newTrackerHandle)
import qualified AtnBot.UserTracker as UT
import Attendance.TimeSheet
import Attendance.BotState
import Control.Lens
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Network.Google as G
import qualified Network.Google.Storage as G
import System.IO
import Web.Slack

newtype AtnBot a = AtnBot (ReaderT AttnH (ResourceT IO) a)
    deriving ( Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch
             , MonadResource, MonadBase IO, MonadMask)

instance MonadBaseControl IO AtnBot where
    type StM AtnBot a = a
    liftBaseWith f = AtnBot $ liftBaseWith $ \run -> f (\(AtnBot x) -> run x)
    restoreM = AtnBot . restoreM

instance G.MonadGoogle Scopes AtnBot where
    liftGoogle (G.Google x) =
        liftResourceT . runReaderT x . googleEnv =<< getAttnH

data AttnH = AttnH
    { slackH :: SlackHandle
    , trackerH :: TrackerHandle
    , dbH :: DBHandle BotStateUpdate BotState
    , googleEnv :: G.Env Scopes
    }

runAtnBot :: SlackConfig -> FilePath -> AtnBot a -> IO a
runAtnBot conf dbPath (AtnBot x) = do
    let initialState = newBotState (defaultTimeSheet timezone deadline)
    putStrLn "Restoring state..."
    dbH <- newDBHandle updateBotState botStateUpdate_ initialState dbPath
    logger <- G.newLogger G.Debug stdout
    googleEnv <- (G.envLogger .~ logger) <$> G.newEnv
    withSlackHandle conf $ \slackH -> do
        trackerH <- newTrackerHandle (getSession slackH) blacklist
        runResourceT $ runReaderT x AttnH{..}

getAttnH :: AtnBot AttnH
getAttnH = AtnBot ask

getBotState :: AtnBot BotState
getBotState = liftIO . getState . dbH =<< getAttnH

modifyBotState :: BotStateUpdate -> AtnBot ()
modifyBotState ev = liftIO . flip commitEvent ev . dbH =<< getAttnH

-------------------------------------------------------------------------------
-- UserTracker

trackUser :: IM -> AtnBot ()
trackUser im = getAttnH >>= \h -> liftIO $ UT.trackUser (trackerH h) im

getTrackedUsers :: AtnBot [UserId]
getTrackedUsers = getAttnH >>= \h -> liftIO $ UT.getTrackedUsers (trackerH h)

channelIsIM :: ChannelId -> AtnBot Bool
channelIsIM cid = getAttnH >>= \h -> liftIO $ UT.channelIsIM (trackerH h) cid

-------------------------------------------------------------------------------
-- Google

uploadFile :: G.MonadGoogle Scopes m => FilePath -> T.Text -> m T.Text
uploadFile filepath name = do
    fileBody <- G.sourceBody filepath
    obj <- G.upload uploadAction fileBody
    return $ fromMaybe (error "uploadFile: no link returned") $ obj ^. G.objMediaLink
  where
    uploadAction =
        G.objectsInsert uploadBucket G.object'
        & (G.oiName .~ Just name)
        . (G.oiPredefinedACL .~ Just G.OIPAPublicRead)

-------------------------------------------------------------------------------
-- Slack helpers

nextEvent :: AtnBot Event
nextEvent = do
    slack <- slackH <$> getAttnH
    liftIO $ getNextEvent slack

sendMsg :: ChannelId -> T.Text -> AtnBot ()
sendMsg cid msg = do
    slack <- slackH <$> getAttnH
    liftIO $ sendMessage slack cid msg

sendRichMsg :: ChannelId -> T.Text -> [Attachment] -> AtnBot ()
sendRichMsg cid msg attnts = do
    slack <- slackH <$> getAttnH
    ret <- liftIO $ sendRichMessage slack cid msg attnts
    either (liftIO . putStrLn . T.unpack) return ret

sendIM :: UserId -> T.Text -> AtnBot ()
sendIM uid msg = do
    tracker <- trackerH <$> getAttnH
    slack <- slackH <$> getAttnH
    liftIO $ UT.lookupIMChannel tracker uid >>= \case
        Just cid -> sendMessage slack cid msg
        Nothing -> putStrLn $ "Couldn't find an IM channel for " ++ show uid

sendRichIM :: UserId -> T.Text -> [Attachment] -> AtnBot ()
sendRichIM uid msg attnts = do
    tracker <- trackerH <$> getAttnH
    slack <- slackH <$> getAttnH
    liftIO $ UT.lookupIMChannel tracker uid >>= \case
        Just cid -> do
            ret <- sendRichMessage slack cid msg attnts
            either (putStrLn . T.unpack) return ret
        Nothing -> putStrLn $ "Couldn't find an IM channel for " ++ show uid

-- | The user must already exist when the connection to slack is
-- established. TODO: get info from UserTracker
getUsername :: UserId -> AtnBot T.Text
getUsername uid =
    maybe "unknown" _userName .
        find (\user -> _userId user == uid) .
            _slackUsers . getSession . slackH <$> getAttnH

getUsernames :: AtnBot (UserId -> T.Text)
getUsernames = do
    SlackSession{..} <- getSession . slackH <$> getAttnH
    return $ \uid ->
        maybe "unknown" _userName $ find (\user -> _userId user == uid) _slackUsers
