{-# LANGUAGE OverloadedStrings #-}
module Mitsfs.Theftcomm.Config
  ( TheftcommConfig(..)
  , TheftcommDest(..)
  ) where

import           Data.Time (Day)

data TheftcommConfig = TheftcommConfig
  { tcDate                :: Day
  , tcSummaryMonths       :: Int
  , tcDoorLogPath         :: String
  , tcKeyholderPath       :: String
  , tcICalendarFolder     :: String
  , tcTheftcommDataFolder :: String
  , tcFromEmail           :: String
  , tcTheftcommEmail      :: String
  , tcKeyholderEmail      :: String
  , tcStarChamberEmail    :: String
  , tcEmailHost           :: String
  , tcShouldEmail         :: Bool
  } deriving (Show, Eq, Ord)

data TheftcommDest = StarChamber | Theftcomm | Keyholders deriving (Show, Eq, Ord)
