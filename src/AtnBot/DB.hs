{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module AtnBot.DB
    ( DBHandle
    , newDBHandle
    , commitEvent
    , getState
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

data DBHandle ev st = DBHandle
    { updateState :: ev -> st -> st
    , serialiseEvent :: Prism' Text ev
    , fileHandle :: Handle
    , curState :: IORef st
    }

newDBHandle :: (ev -> st -> st) -> Prism' Text ev -> st -> FilePath -> IO (DBHandle ev st)
newDBHandle updateState serialiseEvent initialState logPath = do
    putStrLn "Checking log file exists..."
    ensureExists logPath
    putStrLn "Replaying log to restore state..."
    !fileContents <- readFile logPath
    let loggedEvents = lined . T.packed . pre serialiseEvent . _Just
    let !st = foldlOf' loggedEvents (flip updateState) initialState fileContents
    curState <- newIORef st
    putStrLn "Done restoring state"
    fileHandle <- openFile logPath AppendMode
    hSetBuffering fileHandle LineBuffering
    return DBHandle{..}

-- TODO: Escape newlines
commitEvent :: Show ev => DBHandle ev st -> ev -> IO ()
commitEvent DBHandle{..} event = do
    T.hPutStrLn fileHandle (review serialiseEvent event)
    putStrLn $ show event
    modifyIORef curState (updateState event)

getState :: DBHandle ev st -> IO st
getState = readIORef . curState

ensureExists :: FilePath -> IO ()
ensureExists path = do
    exists <- doesFileExist path
    unless exists $ do
        createDirectoryIfMissing True (takeDirectory path)
        writeFile path ""
