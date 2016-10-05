{-# LANGUAGE OverloadedStrings #-}
module Mitsfs.Theftcomm.Validate
  ( allFormattedErrors, canceledHours ) where

import           Control.Lens               ((&), (+~))
import qualified Data.List                  as L
import qualified Data.Map                   as M
import           Data.Maybe
import           Data.Text.Lazy
import           Data.Time                  (Day, TimeZone, UTCTime,
                                             utcToZonedTime)
import           Data.Time.Lens             (days, flexDT)
import           Mitsfs.Theftcomm.ICalendar
import           Mitsfs.Theftcomm.Iterate
import           Text.ICalendar

import           Debug.Trace

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

isCancelled :: RItem VEvent -> Bool
isCancelled r = let
  summary = veSummary (rItem r)
  checkF s = "cancel" `isPrefixOf` toLower (stripStart (summaryValue s))
  in fromMaybe False $ checkF <$> summary

canceledHours :: TimeZone
                  ->  UTCTime -- Start Time
                  -> [RItem VEvent] -- ^ Last weeks events
                  -> [RItem VEvent] -- ^ This weeks events
                  -> [String]
canceledHours tz start old new = let
  end = start & flexDT.days +~ 1
  xs = iterateTuple start end (traceU old) (traceU new)
  showS r s e = "Hours " ++ showRItemSummary r ++ " between " ++ show (utcToZonedTime tz s) ++ " and " ++ show (utcToZonedTime tz e)
  canceledStr :: (UTCTime, UTCTime, Maybe (RItem VEvent), Maybe (RItem VEvent)) -> Maybe String
  canceledStr (_, _, Nothing, Nothing) = Nothing -- Don't do anything
  canceledStr (s, e, _, Just n)
    | isCancelled n = Just $ showS n s e ++ " are cancelled. Please munch."
    | otherwise = Nothing
  canceledStr (s, e, Just f, Nothing) =  Just $ showS f s e ++ " were deleted and should be munched or marked as cancelled."
  in mapMaybe canceledStr xs
