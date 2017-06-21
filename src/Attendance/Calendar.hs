{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Attendance.Calendar
    ( Calendar
    , Expectation(..)
    , Reason

      -- * Creating Calendars
    , weekendsOff
    , markException

      -- * Querying Calendars
    , expectation
    , reason
    , nextExpected
    ) where

import Control.Lens
import Data.AffineSpace
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMS
import Data.Hashable
import Data.Text (Text)
import Data.Thyme
import Data.Thyme.Calendar.WeekDate

-- TODO: Remove once liyang uploads the next version of thyme
instance Hashable Day

data Expectation = ExpectedAt TimeOfDay | NotExpected deriving (Eq, Show)
type Reason = Text

data Calendar = Calendar
    { _calMonday :: Expectation
    , _calTuesday :: Expectation
    , _calWednesday :: Expectation
    , _calThursday :: Expectation
    , _calFriday :: Expectation
    , _calSaturday :: Expectation
    , _calSunday :: Expectation
    , _calExceptions :: HashMap Day (Reason, Expectation)
    } deriving (Eq, Show)

makeLenses ''Calendar

expectation :: Calendar -> Day -> Expectation
expectation cal day =
    case cal ^? calExceptions . ix day of
        Nothing -> cal ^. calRegular day
        Just (_, expect) -> expect

calRegular :: Day -> Lens' Calendar Expectation
calRegular day =
    case day ^. mondayWeek . _mwDay of
        1 -> calMonday
        2 -> calTuesday
        3 -> calWednesday
        4 -> calThursday
        5 -> calFriday
        6 -> calSaturday
        7 -> calSunday
        _ -> error "calRegular: DayOfWeek outside [1,7]"

reason :: Calendar -> Day -> Maybe Reason
reason cal day = cal ^? calExceptions . ix day . _1

-- WARNING: Diverges if never again expected
nextExpected :: Calendar -> Day -> Day
nextExpected cal day =
    case expectation cal day of
        ExpectedAt _ -> day
        NotExpected -> nextExpected cal (day .+^ 1)

markException :: Day -> Reason -> Expectation -> Calendar -> Calendar
markException day reasn expecn = over calExceptions (HMS.insert day (reasn, expecn))

weekendsOff :: TimeOfDay -> Calendar
weekendsOff deadline = Calendar
    { _calMonday = ExpectedAt deadline
    , _calTuesday = ExpectedAt deadline
    , _calWednesday = ExpectedAt deadline
    , _calThursday = ExpectedAt deadline
    , _calFriday = ExpectedAt deadline
    , _calSaturday = NotExpected
    , _calSunday = NotExpected
    , _calExceptions = HMS.empty
    }
