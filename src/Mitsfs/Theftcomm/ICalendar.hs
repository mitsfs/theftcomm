{-# LANGUAGE OverloadedStrings #-}
module Mitsfs.Theftcomm.ICalendar
  ( getICalEvents
  , getICalEventsDays
  , showRItem
  ) where

import           Control.Applicative
import           Control.Lens         ((&), (+~))
import qualified Data.ByteString.Lazy as BS
import           Data.Default
import           Data.List
import           Data.String
import           Data.Time            (Day, LocalTime (..), UTCTime (..),
                                       localTimeToUTC, midnight)
import           Data.Time.Lens       (days, flexDT)

import           Text.ICalendar


showVEvent :: VEvent -> String
showVEvent v = summary (veSummary v) ++ startDate (veDTStart v) ++ endDate (veDTEndDuration v)
  where
    summary (Just s) = show (summaryValue s) ++ " hours "
    summary Nothing  = "<Unknown Hours>"
    startDate (Just d) = showVDateTime (dtStartValue d)
    startDate Nothing  = error "Events should have a start time"
    endDate (Just (Left (DTEnd d _)))         = " - " ++ showVDateTime d
    endDate (Just (Right (DurationProp d _))) = " - " ++ show d
    endDate Nothing                           = ""

showRItem :: RItem VEvent -> String
showRItem r = showVEvent (rItem r)

showVDateTime :: VDateTime -> String
showVDateTime (VDateTime dt)   = showDateTime dt
showVDateTime (VDate (Date d)) = show d

showDateTime :: DateTime -> String
showDateTime (ZonedDateTime dt tz) = show dt ++ " " ++ show tz
showDateTime (FloatingDateTime dt) = show dt
showDateTime (UTCDateTime dt)      = show dt

defaultTZ :: IsString s => s
defaultTZ = "America/New_York"

getICal :: BS.ByteString -> Either String VCalendar
getICal content = do
  vcalL <- parseICalendar def content
  -- error on warnings
  case snd vcalL of
    [] -> Right ()
    xs -> Left $ intercalate "\n" xs
  case uncons (fst vcalL) of
    Nothing      -> Left "VCalendar is empty"
    Just (v, []) -> Right v
    Just (_, _)  -> Left "More than one VCalendar parsed"

toICalEvents :: VCalendar -> [RItem VEvent]
toICalEvents vcal = vEventList (vCalendarTZIDToOffsets vcal defaultTZ) vcal

getICalEvents :: BS.ByteString -> Either String [RItem VEvent]
getICalEvents bs =  toICalEvents <$> getICal bs

getICalEventsDays :: BS.ByteString -> Day -> Int -> Either String [RItem VEvent]
getICalEventsDays content day len = do
    vcal <- getICal content
    tzf <- pure $ vCalendarTZIDToOffsets vcal defaultTZ
    tzOffset <- case tzf (LocalTime day midnight) of
      Just a  -> Right a
      Nothing -> Left $ concat ["Timezone ",  defaultTZ, " is not in calendar Ical"]
    utcTime <- pure $ localTimeToUTC tzOffset $ LocalTime day midnight
    pure $ takeDates utcTime (utcTime & flexDT.days +~ len) $ toICalEvents vcal

takeDates :: UTCTime -> UTCTime -> [RItem r] -> [RItem r]
takeDates l u = takeWhileUpperTime u . filterLowerTime l

filterLowerTime :: UTCTime -> [RItem r] -> [RItem r]
filterLowerTime t = let
  f (Just end) = end >= t
  f Nothing    = False
  date r = rIEndDateUtc r <|> rIStartDateUtc r
  in filter (f . date)

takeWhileUpperTime :: UTCTime -> [RItem r] -> [RItem r]
takeWhileUpperTime t = let
  f (Just start) = start <= t
  f Nothing      = False
  in takeWhile (f . rIStartDateUtc)
