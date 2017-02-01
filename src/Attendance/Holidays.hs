
-- | A value of type 'Holidays' contains a set of non-overlapping
-- 'Holiday's. The interface exported by this module guarantees this
-- invariant.
module Attendance.Holidays
    ( Holidays
    , markHoliday
    , isHoliday
    , getHoliday

    , Holiday(..)
    , toList
    , fromList

    , ppHoliday
    , ppHolidays
    ) where

import Control.Arrow
import Data.AffineSpace
import Data.Function
import Data.List
import Data.Maybe
import Data.Thyme

newtype Holidays = Holidays [Holiday] deriving (Eq, Show)

fromList :: [Holiday] -> Holidays
fromList xs = Holidays $ normaliseHols xs

toList :: Holidays -> [Holiday]
toList (Holidays xs) = xs

instance Monoid Holidays where
    mempty = Holidays []
    mappend (Holidays h1) (Holidays h2) = fromList (h1 ++ h2)

markHoliday :: Day -> Holidays -> Holidays
markHoliday day (Holidays hols) =
    fromList $ Holiday day day : hols

isHoliday :: Day -> Holidays -> Bool
isHoliday day = isJust . getHoliday day

-- TODO: We could improve performance by using the assumption that holidays
-- are ordered to short-circuit the False case as well as the True case.
getHoliday :: Day -> Holidays -> Maybe Holiday
getHoliday day (Holidays hols) = find (day `isWithinHoliday`) hols

-- TODO: Combine across weekends
normaliseHols :: [Holiday] -> [Holiday]
normaliseHols = foldl' mergeWithHead []  . sort
  where
    mergeWithHead (h:rest) x =
        if x `overlaps` h then combine x h : rest else x : h : rest
    mergeWithHead [] x = [x]

-------------------------------------------------------------------------------

-- | A closed interval over days. Bounds are inclusive.
-- TODO: Support part-day holidays.
data Holiday = Holiday { _hFrom :: Day, _hUntil :: Day } deriving (Eq, Show)

instance Ord Holiday where compare = compare `on` (_hFrom &&& _hUntil)

combine :: Holiday -> Holiday -> Holiday
combine (Holiday s1 e1) (Holiday s2 e2) = Holiday (min s1 s2) (max e1 e2)

-- | We consider two holidays to be overlapping, even if one ends the day
-- before the other starts.
overlaps :: Holiday -> Holiday -> Bool
overlaps (Holiday s1 e1) (Holiday s2 e2) =
    case compare s1 s2 of
        LT -> e1 >= s2 .-^ 1
        GT -> e2 >= s1 .-^ 1
        EQ -> True

isWithinHoliday :: Day -> Holiday -> Bool
isWithinHoliday day (Holiday start end) = day >= start && day <= end

ppHoliday :: Holiday -> String
ppHoliday (Holiday start end) = "from " ++ show start ++ " to " ++ show end

ppHolidays :: Holidays -> String
ppHolidays (Holidays hols) = unlines $ map ppHoliday hols
