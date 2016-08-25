{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Attendance.Log
    ( LogHandle
    , newLogHandle
    , logEvent
    , getCurState
    ) where

import Control.Lens
import Control.Monad
import Data.IORef
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text.Lens as T
import System.Directory
import System.FilePath

newtype LogHandle ev st = LogHandle { unLogHandle :: IORef (Handle' ev st) }
data Handle' ev st = Handle'
    { updateState :: ev -> st -> st
    , serialiseEvent :: Prism' Text ev
    , logPath :: FilePath
    , curState :: !st
    }

newLogHandle :: (ev -> st -> st) -> Prism' Text ev -> st -> FilePath -> IO (LogHandle ev st)
newLogHandle updateState serialiseEvent initialState logPath = do
    ensureExists logPath
    let loggedEvents = lined . T.packed . pre serialiseEvent . _Just
    curState <- foldrOf loggedEvents updateState initialState <$> readFile logPath
    let !h = Handle'{..}
    LogHandle <$> newIORef h

-- TODO: Escape newlines
logEvent :: LogHandle ev st -> ev -> IO ()
logEvent (LogHandle h) event = do
    h'@Handle'{..} <- readIORef h
    T.appendFile logPath (review serialiseEvent event <> "\n")
    let curState' = updateState event curState
    writeIORef h $ h'{curState = curState'}

getCurState :: LogHandle ev st -> IO st
getCurState = fmap curState . readIORef . unLogHandle

ensureExists :: FilePath -> IO ()
ensureExists path = do
    exists <- doesFileExist path
    unless exists $ do
        createDirectoryIfMissing True (takeDirectory path)
        writeFile path ""
