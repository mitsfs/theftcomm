{-# LANGUAGE OverloadedStrings #-}
module Mitsfs.Theftcomm.ICalendar
  ( getICalEvents
  , getICalEventsDay
  ) where

import           Control.Applicative
import qualified Data.ByteString.Lazy as BS
import           Data.Default
import           Data.List
import           Data.String
import           Data.Time            (Day, LocalTime (..), UTCTime (..),
                                       addDays, localTimeToUTC, midnight)

import           Text.ICalendar

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
    Just (_, _)  -> Left "More than one VCalendar returned"

toICalEvents :: VCalendar -> [RItem VEvent]
toICalEvents vcal = vEventList (vCalendarTZIDToOffsets vcal defaultTZ) vcal

getICalEvents :: BS.ByteString -> Either String [RItem VEvent]
getICalEvents bs =  toICalEvents <$> getICal bs

getICalEventsDay :: BS.ByteString -> Day -> Either String [RItem VEvent]
getICalEventsDay content day = do
    vcal <- getICal content
    tzf <- pure $ vCalendarTZIDToOffsets vcal defaultTZ
    tzOffset <- case tzf (LocalTime day midnight) of
      Just a  -> Right a
      Nothing -> Left $ concat ["Timezone ",  defaultTZ, " is not in calendar Ical"]
    utcTime <- pure $ localTimeToUTC tzOffset $ LocalTime day midnight
    pure $ filterDay utcTime $ toICalEvents vcal

filterDay :: UTCTime -> [RItem r] -> [RItem r]
filterDay lower = takeDates lower (UTCTime (addDays 1 (utctDay lower)) (utctDayTime lower))

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
