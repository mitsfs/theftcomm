{-# LANGUAGE OverloadedStrings #-}
module Mitsfs.Theftcomm
  ( validate
  , generate
  , summary
  ) where

import qualified Codec.Compression.GZip     as GZip
import           Control.Lens               ((&), (-~))
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy       as BS
import           Data.List
import qualified Data.Map                   as M
import           Data.Maybe
import           Data.Text.Lazy             (Text)
import           Data.Time                  (Day, getCurrentTime,
                                             getCurrentTimeZone, localDay,
                                             utcToLocalTime)
import           Data.Time.Lens             (days, flexD)


import           Mitsfs.Theftcomm.ICalendar
import           Mitsfs.Theftcomm.Validate


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

validate :: Maybe Day -> String -> String -> IO ()
validate day keyholderFile iCalPath = do
  content <- readFileMGzip iCalPath
  tz <- getCurrentTimeZone
  today <- localDay . utcToLocalTime tz <$> getCurrentTime
  keyholders <- decodeKeyholders <$> BS.readFile keyholderFile
  calendar <- pure $ getICalEventsDays content (fromMaybe today day & flexD.days -~ numDaysBack) numDays
  putStr $ either id (intercalate "\n\n") $ allFormattedErrors keyholders <$> calendar

generate :: String -> IO ()
generate = error "Not implemented"

summary  :: String -> IO ()
summary = error "Not implemented"
