{-# LANGUAGE OverloadedStrings #-}
module Mitsfs.Theftcomm.DoorLog
  ( DoorState(..)
  , DoorLog(..)
  , getDoorLog
  ) where

import           Control.Monad           (mzero)
import           Data.ByteString.Char8   (unpack)
import qualified Data.ByteString.Lazy    as BS
import qualified Data.Csv                as Csv
import           Data.Time               (UTCTime, defaultTimeLocale,
                                          parseTimeM)
import qualified Data.Vector             as V

import           Mitsfs.Theftcomm.Config

getDoorLog :: TheftcommConfig -> IO (V.Vector DoorLog)
getDoorLog config = do
  contents <- BS.readFile (tcDoorLogPath config)
  either error pure $ Csv.decode Csv.NoHeader contents

data DoorState = Open | Closed | Unknown deriving (Show, Ord, Eq)

instance Csv.FromField DoorState where
  parseField s
      | s == "open"  = pure Open
      | s == "closed"  = pure Closed
      | s == "unknown"  = pure Unknown
      | otherwise = mzero

data DoorLog = DoorLog
  { doorDate  :: UTCTime
  , doorState :: DoorState
  } deriving (Show, Eq, Ord)

instance Csv.FromField UTCTime where
  parseField s = maybe mzero pure $
    parseTimeM True defaultTimeLocale "%s" (unpack s)

instance Csv.FromRecord DoorLog where
  parseRecord v
    | length v == 4 = DoorLog <$>
                      v Csv..! 1 <*>
                      v Csv..! 2
    | otherwise     = mzero
