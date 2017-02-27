{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The idea here is that when team members know that they're going to be
-- absent, they should enter it in a shared spreadsheet ahead-of-time.
-- Attendancebot periodically downloads this speadsheet to look for
-- changes. The spreadsheet must have the following format:
--
--     [anything] | <user id> | <user id> | ...
--     2017-01-01 |           |         1 | ...
--     2017-01-02 |         1 |         1 | ...
--     2017-01-03 |           |           | ...
--     2017-01-04 |       0.5 |           | ...
--
module AtnBot.Spreadsheet
    ( getAttendanceData
    , ppSpreadsheet
    ) where

import AtnBot.Config
import Attendance.Calendar
import Control.Exception.Lifted
import Control.Lens
import Control.Monad.Trans.Control
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HMS
import Data.Maybe
import Data.Monoid
import Data.Scientific
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Data.Thyme
import qualified Network.Google as G
import qualified Network.Google.Sheets as G
import System.Locale
import Web.Slack.Monad

getAttendanceData
    :: (G.MonadGoogle Scopes m, MonadBaseControl IO m)
    => m (HMS.HashMap (UserId, Day) Double)
getAttendanceData = do
    vr'e <- try $ G.send (G.spreadsheetsValuesGet spreadsheetId spreadsheetRange)
    case vr'e of
        Right vr -> return $ processTable (vr ^. G.vrValues)
        Left (G.TransportError _) -> return HMS.empty
        Left _ -> return HMS.empty

processTable :: [[A.Value]] -> HMS.HashMap (UserId, Day) Double
processTable (uids:attendanceData) = HMS.unions $ map (processRow (processIdRow uids)) attendanceData
processTable _ = error "No slack UID row"

processIdRow :: [A.Value] -> [Maybe UserId]
processIdRow vals = map (validateId . fromStringVal) $ tail vals
  where
    validateId x
        | T.head x /= 'U' = Nothing
        | T.length x /= 9 = Nothing
        | otherwise = Just $ Id x

processRow :: [Maybe UserId] -> [A.Value] -> HMS.HashMap (UserId, Day) Double
processRow userColumns (dayVal:dataVals) = HMS.fromList
    [ ((uid, fromDayVal dayVal), val)
    | (Just uid, Just val) <- zip userColumns (map fromDoubleVal dataVals ++ repeat Nothing)
    ]
processRow _ _ = error "processRow: No day column"

fromDoubleVal :: A.Value -> Maybe Double
fromDoubleVal (A.Number x) = Just $ toRealFloat x
fromDoubleVal (A.String x) = either (const Nothing) (Just . fst) $ T.double x
fromDoubleVal _ = Nothing

fromDayVal :: A.Value -> Day
fromDayVal =
    fromMaybe (error "processRow: couldn't parse day") .
    parseTime defaultTimeLocale "%Y-%m-%d" .
    T.unpack . T.takeWhile (/= '(') . fromStringVal

fromStringVal :: A.Value -> T.Text
fromStringVal (A.String x) = T.strip x
fromStringVal x = error "fromStringVal: not a string: " <> T.pack (show x)

-------------------------------------------------------------------------------

ppSpreadsheet :: (UserId -> T.Text) -> HMS.HashMap (UserId, Day) Double -> T.Text
ppSpreadsheet username xs = T.unlines $
    [ "Raw data from spreadsheet:"
    , "```"
    ] ++ map ppRawRow (HMS.toList xs) ++
    [ "```"
    , "Processed data:"
    , "```"
    ] ++ map ppProcRow (HMS.toList $ spreadsheetToHolidays xs) ++
    [ "```"
    ]
  where
    ppRawRow ((uid, day), offtime) =
        username uid <> " on " <> T.pack (show day) <> ": " <> T.pack (show offtime)
    ppProcRow (uid, cal) = "[" <> username uid <> "]\n" <> T.pack (show cal)
    spreadsheetToHolidays :: HMS.HashMap (UserId, Day) Double -> HMS.HashMap UserId Calendar
    spreadsheetToHolidays = HMS.foldrWithKey (\(uid, day) _ -> over (at uid . non (weekendsOff deadline)) (markException day "" NotExpected)) HMS.empty
