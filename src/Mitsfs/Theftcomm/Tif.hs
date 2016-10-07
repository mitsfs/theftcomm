{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Mitsfs.Theftcomm.Tif
  (
  ) where

import           Data.Csv
import           Data.Text.Lazy (Text)
import           GHC.Generics   (Generic)

data Keyholder = Keyholder Text deriving (Show, Eq, Ord, Generic)

instance FromField Keyholder where
  parseField a = Keyholder <$> parseField a

instance ToField Keyholder where
  toField (Keyholder k) = toField k

data TifEntry a = TifEntry Keyholder a deriving (Show, Eq, Ord, Generic)
instance FromNamedRecord a => FromNamedRecord (TifEntry a) where
    parseNamedRecord m = TifEntry <$> m .: "keyholder" <*> parseNamedRecord m
instance ToNamedRecord a => ToNamedRecord (TifEntry a) where
    toNamedRecord (TifEntry keyholder tif) = mappend
      (namedRecord ["keyholder" .= keyholder])
      (toNamedRecord tif)
instance DefaultOrdered a => DefaultOrdered (TifEntry a) where
  headerOrder (TifEntry _ v) = mappend (header ["keyholder"]) (headerOrder v)

data Hours = Scheduled String
  | Canceled String
  | Unscheduled deriving (Show, Ord, Eq)

data TifLog = TifLog
  { rawScheduledHeld               :: Float
  , rawScheduledMunched            :: Float
  , rawScheduledCanceled           :: Float
  , rawScheduledCanceledImproperly :: Float
  , rawScheduledAbsent             :: Float
  , rawUnscheduledHeld             :: Float
  } deriving (Show, Ord, Eq, Generic)

instance FromNamedRecord TifLog
instance ToNamedRecord TifLog
instance DefaultOrdered TifLog

instance Monoid TifLog where
  mempty = TifLog 0 0 0 0 0 0
  mappend a b = TifLog
      (rawScheduledHeld a + rawScheduledHeld b)
      (rawScheduledMunched a + rawScheduledMunched b)
      (rawScheduledCanceled a + rawScheduledCanceled b)
      (rawScheduledCanceledImproperly a + rawScheduledCanceledImproperly b)
      (rawScheduledAbsent a + rawScheduledAbsent b)
      (rawUnscheduledHeld a + rawUnscheduledHeld b)

data TifLogSummary = TifLogSummary
  { tifScore           :: Float
  , summaryHeld        :: Float -- Includes munched
  , summaryCanceled    :: Float -- Includes Improperly canceled and absent
  , summaryUnscheduled :: Float
  , summaryAbsent      :: Float -- Includes improperly canceled and absent
  } deriving (Show, Ord, Eq, Generic)

instance FromNamedRecord TifLogSummary
instance ToNamedRecord TifLogSummary
instance DefaultOrdered TifLogSummary

rawToSummary :: TifLog -> TifLogSummary
rawToSummary (TifLog held munched canceled canceledI absent unscheduled) =
  TifLogSummary {
    tifScore = held + munched - canceled - 2 * (canceledI + absent) + unscheduled / 2,
    summaryHeld = held + munched,
    summaryCanceled = canceled + canceledI + absent,
    summaryUnscheduled = unscheduled,
    summaryAbsent = absent
    }
