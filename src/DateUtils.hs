module DateUtils(
    dateBallistaToJulian
    , timeBallistaToTimeOfDay
    , datetimeBallistaToLocalTime) where

import Data.Time
import Data.Time.Calendar
import Data.Fixed

{- | Converts a ballista formatted string to a julian day.
   Expects the string in standard ballista format "YYYYMMDD"
-}
dateBallistaToJulian :: String -> Day
dateBallistaToJulian dt = if length dt < 8
  then error "Invalid date string length"
  else fromGregorian ys ms ds
  where ys = (read (take 4 dt)) :: Integer
        ms = (read (take 2 (drop 4 dt))) :: Int
        ds = (read (take 2 (drop 6 dt))) :: Int

datetimeBallistaToLocalTime :: String -> LocalTime
datetimeBallistaToLocalTime dt = if length dt < 14
  then error "Invalid datetime string length"
  else LocalTime (dateBallistaToJulian dt) (timeBallistaToTimeOfDay (drop 8 dt))

{- | Converts a ballista format time to a TimeOfDay.
Expects the time string in "HHMMSS"
-}
timeBallistaToTimeOfDay :: String -> TimeOfDay
timeBallistaToTimeOfDay ts = if length ts < 6
  then error "Invalid time string length"
  else TimeOfDay hs ms ss
  where hs = (read (take 2 ts)) :: Int
        ms = (read (take 2 (drop 2 ts))) :: Int
        ss = (read (take 2 (drop 4 ts))) :: Data.Fixed.Pico
