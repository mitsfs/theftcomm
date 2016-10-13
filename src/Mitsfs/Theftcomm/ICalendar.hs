{-# LANGUAGE OverloadedStrings #-}
module Mitsfs.Theftcomm.ICalendar
  ( getICalEvents
  , getICalEventsDays
  , getTZOffset
  , showRItem
  , showRItemSummary
  , toUTC
  ) where

import           Control.Applicative
import           Control.Lens         ((&), (+~))
import qualified Data.ByteString.Lazy as BS
import           Data.Default
import           Data.List
import           Data.Maybe
import           Data.String
import           Data.Time            (Day, LocalTime (..), TimeZone,
                                       UTCTime (..), localTimeToUTC, midnight,
                                       utcToZonedTime)
import           Data.Time.Lens       (days, flexDT)

import           Text.ICalendar

showVEvent :: (Day -> TimeZone) -> VEvent -> String
showVEvent tzf v = summary (veSummary v) ++ startDate (veDTStart v) ++ endDate (veDTEndDuration v)
  where
    summary (Just s) = show (summaryValue s) ++ " hours "
    summary Nothing  = "<Unknown Hours>"
    startDate (Just d) = showVDateTime tzf (dtStartValue d)
    startDate Nothing  = error "Events should have a start time"
    endDate (Just (Left (DTEnd d _)))         = " - " ++ showVDateTime tzf d
    endDate (Just (Right (DurationProp d _))) = " - " ++ show d
    endDate Nothing                           = ""

showRItem :: (Day -> TimeZone) -> RItem VEvent -> String
showRItem tzf r = showVEvent tzf (rItem r)

showRItemSummary :: RItem VEvent -> String
showRItemSummary r = summary (veSummary (rItem r))
  where
    summary (Just s) = show (summaryValue s) ++ " hours "
    summary Nothing  = "<Unknown Hours>"

showVDateTime :: (Day -> TimeZone) -> VDateTime -> String
showVDateTime tzf (VDateTime dt) = showDateTime tzf dt
showVDateTime _ (VDate (Date d)) = show d

showDateTime :: (Day -> TimeZone) -> DateTime -> String
showDateTime _ (ZonedDateTime dt tz) = show dt ++ " " ++ show tz
showDateTime _ (FloatingDateTime dt) = show dt
showDateTime tzf (UTCDateTime dt)     = show $ utcToZonedTime (tzf $ utctDay dt) dt

defaultTZ :: IsString s => s
defaultTZ = "America/New_York"

getTZOffset :: BS.ByteString -> Day -> TimeZone
getTZOffset content d = let
    vcal = either (error "Invalid VCalendar") id $ getICal content
    lt = LocalTime d midnight
    in fromMaybe (error "Invalid Date or TimeZone") $ vCalendarTZIDToOffsets vcal defaultTZ lt

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

toUTC :: Day -> TimeZone -> UTCTime
toUTC d tz = localTimeToUTC tz (LocalTime d midnight)

getICalEventsDays :: BS.ByteString -> Day -> Int -> Either String [RItem VEvent]
getICalEventsDays content day len = do
    vcal <- getICal content
    let tzf = vCalendarTZIDToOffsets vcal defaultTZ
    tzOffset <- case tzf (LocalTime day midnight) of
      Just a  -> Right a
      Nothing -> Left $ concat ["Timezone ",  defaultTZ, " is not in calendar Ical"]
    let utcTime = localTimeToUTC tzOffset $ LocalTime day midnight
    pure $ takeDates utcTime (utcTime & flexDT.days +~ len) $ toICalEvents vcal

takeDates :: UTCTime -> UTCTime -> [RItem a] -> [RItem a]
takeDates l u = takeWhileUpperTime u . filterLowerTime l

filterLowerTime :: UTCTime -> [RItem a] -> [RItem a]
filterLowerTime t = let
  f (Just end) = end >= t
  f Nothing    = error "No start or end Time"
  date r = rIEndDateUtc r <|> error "VEVENTS should have end days"
  in filter (f . date)

takeWhileUpperTime :: UTCTime -> [RItem a] -> [RItem a]
takeWhileUpperTime t = let
  f (Just start) = start <= t
  f Nothing      = error "No start time"
  in takeWhile (f . rIStartDateUtc)
