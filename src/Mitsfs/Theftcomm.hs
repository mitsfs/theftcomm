{-# LANGUAGE OverloadedStrings #-}
module Mitsfs.Theftcomm
  ( validate
  , generate
  , summary
  , TheftcommConfig(..)
  ) where

import qualified Codec.Compression.GZip     as GZip
import           Control.Lens               ((&), (-~))
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy       as BS
import           Data.List
import qualified Data.Map                   as M
import           Data.Maybe
import           Data.Text.Lazy             (Text)
import           Data.Time                  (Day, utcToLocalTime)
import           Data.Time.Lens             (days, flexD)


import           Mitsfs.Theftcomm.ICalendar
import           Mitsfs.Theftcomm.Validate


data TheftcommConfig = TheftcommConfig
  { tcDate                :: Day
  , tcKeyholderPath       :: String
  , tcICalendarFolder     :: String
  , tcTheftcommDataFolder :: String
  , tcFromEmail           :: String
  , tcTheftcommEmail      :: String
  , tcKeyHolderEmail      :: String
  , tcStarChamberEmail    :: String
  , tcShouldEmail         :: Bool
  } deriving (Show, Eq, Ord)

readFileMGzip :: String -> IO BS.ByteString
readFileMGzip path = (if ".gz" `isSuffixOf` path then GZip.decompress else id)
                       <$> BS.readFile path

numDaysBack :: Integral a => a
numDaysBack = 60

numDaysForward :: Integral a => a
numDaysForward = 366

numDays :: Integral a => a
numDays = numDaysForward + numDaysBack

decodeKeyholders :: BS.ByteString -> M.Map Text Text
decodeKeyholders bs = fromMaybe (error "Couldn't decode Keyholders file") (A.decode bs)

validate :: TheftcommConfig -> IO ()
validate config = do
  content <- readFileMGzip (tcICalendarFolder config)
  keyholders <- decodeKeyholders <$> BS.readFile (tcKeyholderPath config)
  startDay <- pure $ tcDate config & flexD.days -~ numDaysBack
  calendar <- pure $ getICalEventsDays content startDay numDays
  output <- pure $ either id (intercalate "\n\n") $ allFormattedErrors keyholders <$> calendar
  if null output then pure () else putStrLn output

generate :: TheftcommConfig -> IO ()
generate = error "Not implemented"

summary  :: TheftcommConfig -> IO ()
summary = error "Not implemented"
