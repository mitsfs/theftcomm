{-# LANGUAGE OverloadedStrings #-}
module Mitsfs.Theftcomm.Validate
  ( allFormattedErrors, canceledHours, generateHours, summaryHours ) where

import           Control.Lens               ((&), (+~))
import qualified Data.List                  as L
import qualified Data.Map                   as M
import           Data.Maybe
import           Data.Text.Lazy
import           Data.Time                  (Day, TimeZone, UTCTime,
                                             diffUTCTime, utcToZonedTime)
import           Data.Time.Lens             (days, flexDT)
import qualified Data.Vector                as V
import           Mitsfs.Theftcomm.DoorLog
import           Mitsfs.Theftcomm.ICalendar
import           Mitsfs.Theftcomm.Iterate
import           Mitsfs.Theftcomm.Tif
import           Text.ICalendar             (RItem (..), VEvent (..),
                                             summaryValue)

allFormattedErrors :: (Day -> TimeZone) -> M.Map Text Text -> [RItem VEvent] -> [String]
allFormattedErrors tzf k vs = catMaybes
          [formattedErrors (badSummaries tzf k vs) "Invalid calendar summaries:"
          , formattedErrors (overlapingEvents tzf vs) "Overlaping hours at:"]

overlapingEvents :: (Day -> TimeZone) -> [RItem VEvent] -> [String]
overlapingEvents tzf (x:y:xs) = if rIStartDateUtc y < rIEndDateUtc x then
  (showRItem tzf x ++ " and " ++ showRItem tzf y) : overlapingEvents tzf (y:xs)
  else overlapingEvents tzf (y:xs)
overlapingEvents _ _ = []

formattedErrors :: [String] -> String -> Maybe String
formattedErrors [] _ = Nothing
formattedErrors xs t = Just $ L.intercalate "\n    " (t:xs)

badSummaries :: (Day -> TimeZone) -> M.Map Text Text -> [RItem VEvent] -> [String]
badSummaries tzf key = Prelude.foldr ((++) . badSummary tzf key) []

badSummary :: (Day -> TimeZone) -> M.Map Text Text -> RItem VEvent -> [String]
badSummary tzf key v = summaryError (veSummary (rItem v))
  where
    summaryError Nothing  = ["Blank Keyholder: " ++ showRItem tzf v]
    summaryError (Just s) = f $ split (== ':') (summaryValue s)
    f []       = error "Should not have an empty split"
    f [k]   = checkKey k
    f [x,k] = checkKey k ++ checkCanceled x
    f (x:k:xs) = checkKey k ++ checkCanceled x ++ [" Unknown part " ++ show xs ++ extra]
    extra = " in " ++ showRItem tzf v
    checkKey k = if M.member (strip k) key then [] else ["Unknown Keyholder " ++ show k ++ extra]
    checkCanceled c
      | c == "Canceled" = []
      | toLower c == "canceled" = ["Canceled capitalixed incorrectly" ++ extra]
      | toLower c == "cancelled" = ["Canceled spelled incorrectly (Use American version)" ++ extra]
      | otherwise = ["Unknown prefix: " ++ show c ++ extra]


canceledHours :: TimeZone
                  ->  UTCTime -- Start Time
                  -> [RItem VEvent] -- ^ Last weeks events
                  -> [RItem VEvent] -- ^ This weeks events
                  -> [String]
canceledHours tz start old new = let
  end = start & flexDT.days +~ 1
  xs = iterateTuple start end old new
  showS (Keyholder k) s e = "Hours " ++ unpack k ++ " between " ++ show (utcToZonedTime tz s) ++ " and " ++ show (utcToZonedTime tz e)
  canceledStr :: (UTCTime, UTCTime, Maybe (RItem VEvent), Maybe (RItem VEvent)) -> Maybe String
  canceledStr (s, e, a, b) = case (toHours a, toHours b) of
    (Unscheduled, Unscheduled) -> Nothing
    (Scheduled k, Unscheduled) -> Just $ showS k s e ++ " were deleted and should be munched or marked as cancelled."
    (Canceled k, Unscheduled) -> Nothing -- Should we do nothing here?
    (_, Scheduled _) -> Nothing
    (_, Canceled k) -> Just $ showS k s e ++ " are cancelled. Please munch."
  in mapMaybe canceledStr xs

generateHours :: TimeZone
                  ->  UTCTime -- Start Time
                  -> [RItem VEvent] -- ^ Last weeks events
                  -> [RItem VEvent] -- ^ This weeks events
                  -> V.Vector DoorLog -- ^ DoorLog
                  -> [TifEntry TifLog]
generateHours tz start old new doorLog = let
  end = start & flexDT.days +~ 1
  xs = iterate3Tuple start end old new doorLog
  summary :: (UTCTime, UTCTime, Maybe (RItem VEvent), Maybe (RItem VEvent), Maybe DoorLog) -> Maybe (TifEntry TifLog)
  summary (_, _ , _, _, Nothing) = error "When generating summary should have a door log"
  summary (s, e, a, b, Just l) = let
    len = fromRational $ toRational (diffUTCTime e s) / 3600
    toT k t = Just $ TifEntry k t
    in case (toHours a, toHours b, doorState l) of
      (_, _, Unknown) -> Nothing
      (Unscheduled, Unscheduled, _) -> Nothing
      (Scheduled _, Scheduled k, Closed) -> toT k $ mempty {rawScheduledAbsent = len}
      (Scheduled _, Scheduled k, Open) -> toT k $ mempty {rawScheduledHeld = len}
      (Scheduled k, Unscheduled, Closed) -> toT k $ mempty {rawScheduledCanceledImproperly = len}
      (Scheduled k, Unscheduled, Open) -> toT k $ mempty {rawScheduledHeld = len}
      (Scheduled _, Canceled k, Closed) -> toT k $ mempty {rawScheduledCanceled = len}
      (Scheduled _, Canceled k, Open) -> toT k $ mempty {rawScheduledHeld = len}
      (Unscheduled, Scheduled k, Open) -> toT k $ mempty {rawUnscheduledHeld = len}
      (Unscheduled, Scheduled k, Closed) -> toT k $ mempty {rawScheduledAbsent = len}
      (Unscheduled, Canceled k, Open) -> toT k $ mempty {rawUnscheduledHeld = len}
      (Unscheduled, Canceled k, Closed) -> toT k $ mempty {rawScheduledCanceled = len}
      (Canceled _, Scheduled k, Closed) -> toT k $ mempty {rawScheduledAbsent = len}
      (Canceled _, Scheduled k, Open) -> toT k $ mempty {rawScheduledHeld = len}
      (Canceled k, Unscheduled, Closed) -> Nothing
      (Canceled k, Unscheduled, Open) -> Nothing
      (Canceled _, Canceled k, Closed) -> toT k $ mempty {rawScheduledCanceled = len}
      (Canceled _, Canceled k, Open) -> toT k $ mempty {rawScheduledHeld = len}
  in tifConcat $ mapMaybe summary xs

summaryHours :: [TifEntry TifLog] -> [TifEntry TifLogSummary]
summaryHours xs = let
  toS (TifEntry k v) = TifEntry k (rawToSummary v)
  in toS <$> tifConcat xs
