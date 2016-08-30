{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Attendance.Spreadsheet
    ( getAttendanceData
    , updateFromSpreadsheet
    , ppSpreadsheet
    ) where

import Attendance.Monad
import Attendance.TimeSheet
import Control.Exception.Lifted
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Control
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HMS
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Data.Thyme.Calendar
import Data.Thyme.Format
import qualified Network.Google as G
import qualified Network.Google.Sheets as G
import System.Locale
import Web.Slack.Monad

updateFromSpreadsheet :: HMS.HashMap (UserId, Day) Double -> Attendance ()
updateFromSpreadsheet xs = do
    ts <- getTimeSheet
    forM_ (HMS.toList xs) $ \((uid, day), amt) ->
        case lookupTiming ts uid day of
            _ | amt <= 0 -> return ()
            OnHoliday -> return ()
            _ -> markHoliday uid day amt

ppSpreadsheet :: HMS.HashMap (UserId, Day) Double -> Attendance T.Text
ppSpreadsheet xs = do
    rows <- forM (HMS.toList xs) $ \((uid, day), offtime) -> do
      uname <- getUsername uid
      return $ "  " <> uname <> " on " <> T.pack (show day) <> ": " <> T.pack (show offtime)
    return $ T.unlines $ [ "```" ] ++ rows ++ [ "```" ]

getAttendanceData
    :: (G.MonadGoogle Scopes m, MonadBaseControl IO m)
    => m (HMS.HashMap (UserId, Day) Double)
getAttendanceData = do
    let sheetId = ""
    vr'e <- try $ G.send (G.spreadsheetsValuesGet sheetId "")
    case vr'e of
        Right vr -> return $ HMS.unions $ map processRow $ vr ^. G.vrValues
        Left (G.TransportError _) -> return HMS.empty
        Left _ -> return HMS.empty

processRow :: [A.Value] -> HMS.HashMap (UserId, Day) Double
processRow vals =
    HMS.fromList
        [ ((uid,day), either error fst (T.double val))
        | (Just uid, val) <- zip userColumns (xs ++ repeat "")
        , not (T.null val)
        ]
  where
    (dayStr:xs) = map fromString vals
    day = fromMaybe (error "processRow: couldn't parse day") $
            parseTime defaultTimeLocale "%Y-%m-%d" $
            T.unpack $ T.takeWhile (/= '(') dayStr
    fromString (A.String x) = T.strip x
    fromString x = error "processRow: not a string:" <> T.pack (show x)

-- Order matters - must be the same as in the spreadsheet.
userColumns :: [Maybe UserId]
userColumns = []
