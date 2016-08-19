{-# LANGUAGE OverloadedStrings #-}

-- | This module contains functionality for recoding user check-ins. All
-- check-in events are recorded, and times are represented in
-- a timezone-independent way. cf. Attendance.TimeSheet.
module Attendance.CheckIn
    ( CheckIn(..)
    , writeCheckIn
    , readCheckIns
    ) where

import Control.Lens
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lens as T
import Data.Thyme
import System.Locale
import Web.Slack hiding (lines)

data CheckIn = CheckIn { ciUser :: UserId, ciTimestamp :: UTCTime }

instance Show CheckIn where
    show (CheckIn (Id uid) ts) = unwords
        [ "Check-in:", T.unpack uid, "at"
        , formatTime defaultTimeLocale "%H:%M" ts
        ]

writeCheckIn :: FilePath -> CheckIn -> IO ()
writeCheckIn fp ci =
    T.appendFile fp (review checkIn ci <> "\n")

readCheckIns :: FilePath -> IO [CheckIn]
readCheckIns fp =
    toListOf (lined . T.packed . pre checkIn . _Just) <$> readFile fp

checkIn :: Prism' T.Text CheckIn
checkIn = prism' pp parse
  where
    pp (CheckIn (Id uid) ts) = review posixTime ts <> "\t" <> uid
    parse txt = case T.splitOn "\t" txt of
        (x:y:_) -> CheckIn (Id y) <$> preview posixTime x
        _ -> Nothing

posixTime :: Prism' T.Text UTCTime
posixTime = prism' pp parse
  where
    pp = T.pack . formatTime defaultTimeLocale "%s"
    parse = parseTime defaultTimeLocale "%s" . T.unpack
