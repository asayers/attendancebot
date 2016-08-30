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
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text.Lens as T
import System.Directory
import System.FilePath
import System.IO

data LogHandle ev st = LogHandle
    { updateState :: ev -> st -> st
    , serialiseEvent :: Prism' Text ev
    , logHandle :: Handle
    , curState :: IORef st
    }

newLogHandle :: (ev -> st -> st) -> Prism' Text ev -> st -> FilePath -> IO (LogHandle ev st)
newLogHandle updateState serialiseEvent initialState logPath = do
    putStrLn "Checking log file exists..."
    ensureExists logPath
    putStrLn "Replaying log to restore state..."
    let loggedEvents = lined . T.packed . pre serialiseEvent . _Just
    st <- foldlOf' loggedEvents (flip updateState) initialState <$> readFile logPath
    curState <- newIORef st
    putStrLn "Done restoring state"
    logHandle <- openFile logPath WriteMode
    return LogHandle{..}

-- TODO: Escape newlines
logEvent :: Show ev => LogHandle ev st -> ev -> IO ()
logEvent LogHandle{..} event = do
    T.hPutStrLn logHandle (review serialiseEvent event)
    putStrLn $ show event
    modifyIORef curState (updateState event)

getCurState :: LogHandle ev st -> IO st
getCurState = readIORef . curState

ensureExists :: FilePath -> IO ()
ensureExists path = do
    exists <- doesFileExist path
    unless exists $ do
        createDirectoryIfMissing True (takeDirectory path)
        writeFile path ""
