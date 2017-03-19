{-# LANGUAGE OverloadedStrings #-}
module Mitsfs.Theftcomm.ICalendar
  ( defaultTZ
  , getICalEvents
  , getICalEventsDays
  , getTZOffset
  , showRItem
  , showRItemSummary
  , showDateTimePair
  , toUTC
  ) where

import           Control.Applicative
import           Control.Lens         ((&), (+~))
import qualified Data.ByteString.Lazy as BS
import           Data.Default
import           Data.List
import           Data.Maybe
import           Data.String
import           Data.Text.Lazy       (Text)
import           Data.Time            (Day, LocalTime (..), TimeOfDay (..),
                                       TimeZone, UTCTime (..), localTimeToUTC,
                                       midnight, timeZoneOffsetString,
                                       utcToLocalTime)
import           Data.Time.Lens       (days, flexDT)

import           Text.ICalendar

showVEvent :: (Text -> Day -> TimeZone) -> VEvent -> String
showVEvent tzf v = summary (veSummary v) ++ date (veDTStart v) (veDTEndDuration v)
  where
    summary (Just s) = show (summaryValue s) ++ " hours "
    summary Nothing  = "<Unknown Hours>"
    date Nothing _  = error "Events should have a start time"
    date (Just d) Nothing = showVDateTime tzf (dtStartValue d)
    date (Just d) (Just (Left (DTEnd de _))) = showVDateTimePair tzf (dtStartValue d) de
    date (Just d) (Just (Right (DurationProp de _))) = showVDateTime tzf (dtStartValue d) ++ " - " ++ show de

showRItem :: (Text -> Day -> TimeZone) -> RItem VEvent -> String
showRItem tzf r = showVEvent tzf (rItem r)

showRItemSummary :: RItem VEvent -> String
showRItemSummary r = summary (veSummary (rItem r))
  where
    summary (Just s) = show (summaryValue s) ++ " hours "
    summary Nothing  = "<Unknown Hours>"

showVDateTimePair :: (Text -> Day -> TimeZone) -> VDateTime -> VDateTime -> String
showVDateTimePair tzf (VDateTime dt) (VDateTime dte) = showDateTimePair tzf dt dte
showVDateTimePair _ (VDate (Date d))  (VDate (Date de)) = show d ++ show de
showVDateTimePair _ s e = error "Miss Matched date types" ++ show s ++ "-" ++ show e

showDateTimePair :: (Text -> Day -> TimeZone) -> DateTime -> DateTime -> String
showDateTimePair tzf (ZonedDateTime dt stz) (ZonedDateTime dte stze)
  | tz /= tze = show dt ++ tz ++ "-" ++ show dte ++ tze
  | otherwise = showDTPair dt dte ++ tz
  where
    tz = showTZ $ tzf stz $ localDay dt
    tze = showTZ $ tzf stze $ localDay dte
showDateTimePair _ (FloatingDateTime dt) (FloatingDateTime dte) = showDTPair dt dte
showDateTimePair tzf (UTCDateTime dt) (UTCDateTime dte) = let
  tz = tzf defaultTZ $ utctDay dt
  tze = tzf defaultTZ $ utctDay dte
  dtS = utcToLocalTime tz dt
  dtE = utcToLocalTime tze dte
  stz = showTZ tz
  stze = showTZ tze
  in if stz /= stze then  show dtS ++ stz ++ "-" ++ show dtE ++ stze ++ utcDebug
  else showDTPair dtS dtE ++ stz ++ utcDebug
showDateTimePair _ dt dte = error "Date time pair types should match" ++ show dt ++ show dte

showDTPair :: LocalTime -> LocalTime -> String
showDTPair (LocalTime ds ts) (LocalTime de te)
  | ds == de = show ds ++ " " ++ showTime ts ++ "-" ++ showTime te
  | otherwise = show ds ++ " " ++ showTime ts ++ "-" ++ show de ++ " " ++ showTime te

showTime :: TimeOfDay -> String
showTime (TimeOfDay h m _) = show2 h ++ ":" ++ show2 m

show2 :: Int -> String
show2 i = if i < 10 then "0" ++ show i else show i

showTZ :: TimeZone -> String
showTZ tz = " T" ++ timeZoneOffsetString tz

showVDateTime :: (Text -> Day -> TimeZone) -> VDateTime -> String
showVDateTime tzf (VDateTime dt) = showDateTime tzf dt
showVDateTime _ (VDate (Date d)) = show d

showDateTime :: (Text -> Day -> TimeZone) -> DateTime -> String
showDateTime _ (ZonedDateTime dt tz) = show dt ++ " " ++ show tz
showDateTime _ (FloatingDateTime dt) = show dt
showDateTime tzf (UTCDateTime dt)    = let tz = tzf defaultTZ $ utctDay dt
  in show (utcToLocalTime tz dt) ++ showTZ tz ++ utcDebug

defaultTZ :: IsString s => s
defaultTZ = "America/New_York"

utcDebug :: IsString s => s
utcDebug = "'"

getTZOffset :: BS.ByteString -> Text -> Day -> TimeZone
getTZOffset content t d = let
    vcal = either (error "Invalid VCalendar") id $ getICal content
    lt = LocalTime d midnight
    in fromMaybe (error "Invalid Date or TimeZone") $ vCalendarTZIDToOffsets vcal t lt

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
