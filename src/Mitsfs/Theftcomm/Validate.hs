{-# LANGUAGE OverloadedStrings #-}
module Mitsfs.Theftcomm.Validate
  ( allFormattedErrors ) where

import qualified Data.List                  as L
import qualified Data.Map                   as M
import           Data.Maybe
import           Data.Text.Lazy
import           Mitsfs.Theftcomm.ICalendar
import           Text.ICalendar

allFormattedErrors :: M.Map Text Text -> [RItem VEvent] -> [String]
allFormattedErrors k vs = catMaybes
          [formattedErrors (badSummaries k vs) "Invalid calendar summaries:"
          , formattedErrors (overlapingEvents vs) "Overlaping hours at:"]

overlapingEvents :: [RItem VEvent] -> [String]
overlapingEvents (x:y:xs) = if rIStartDateUtc y < rIEndDateUtc x then
  (showRItem x ++ " and " ++ showRItem y) : overlapingEvents (y:xs)
  else overlapingEvents (y:xs)
overlapingEvents _ = []

formattedErrors :: [String] -> String -> Maybe String
formattedErrors [] _ = Nothing
formattedErrors xs t = Just $ L.intercalate "\n    " (t:xs)

badSummaries :: M.Map Text Text -> [RItem VEvent] -> [String]
badSummaries key = Prelude.foldr ((++) . badSummary key) []

badSummary :: M.Map Text Text -> RItem VEvent -> [String]
badSummary key v = summaryError (veSummary (rItem v))
  where
    summaryError Nothing  = ["Blank Keyholder: " ++ showRItem v]
    summaryError (Just s) = f $ split (== ':') (summaryValue s)
    f []       = error "Should not have an empty split"
    f [k]   = checkKey k
    f [x,k] = checkKey k ++ checkCanceled x
    f (x:k:xs) = checkKey k ++ checkCanceled x ++ [" Unknown part " ++ show xs ++ extra]
    extra = " in " ++ showRItem v
    checkKey k = if M.member (strip k) key then [] else ["Unknown Keyholder " ++ show k ++ extra]
    checkCanceled c
      | c == "Canceled" = []
      | toLower c == "canceled" = ["Canceled capitalixed incorrectly" ++ extra]
      | toLower c == "cancelled" = ["Canceled spelled incorrectly (Use American version)" ++ extra]
      | otherwise = ["Unknown prefix: " ++ show c ++ extra]
