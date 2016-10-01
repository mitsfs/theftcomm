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
import           Data.Text.Lazy             (Text, pack)
import           Data.Time                  (Day, UTCTime, defaultTimeLocale,
                                             formatTime)
import           Data.Time.Lens             (days, flexD)
import           Network.HaskellNet.SMTP

import           Mitsfs.Theftcomm.Config
import           Mitsfs.Theftcomm.DoorLog
import           Mitsfs.Theftcomm.ICalendar
import           Mitsfs.Theftcomm.Validate


outputIO :: TheftcommConfig -> TheftcommDest -> String -> String -> IO ()
outputIO config dest subject xs
  | null xs = pure ()
  | tcShouldEmail config = let
      server       = "outgoing.mit.edu"
      from         = tcFromEmail config
      to           = case dest of
        StarChamber -> tcStarChamberEmail config
        Theftcomm   -> tcTheftcommEmail config
        Keyholders  -> tcKeyholderEmail config
      in doSMTP server $ \conn ->
        sendPlainTextMail to from subject (pack xs) conn
  | otherwise = putStrLn xs

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

icalFileName :: Day -> String
icalFileName = formatTime defaultTimeLocale "/library-schedule-%F.ics.gz"

summaryFileName :: Day -> String
summaryFileName = formatTime defaultTimeLocale "/tif-data-%F.csv"

validate :: TheftcommConfig -> IO ()
validate config = do
  content <- readFileMGzip (tcICalendarFolder config ++ icalFileName (tcDate config))
  keyholders <- decodeKeyholders <$> BS.readFile (tcKeyholderPath config)
  let startDay = tcDate config & flexD.days -~ numDaysBack
  let calendar = getICalEventsDays content startDay numDays
  let output = either id (intercalate "\n\n") $ allFormattedErrors keyholders <$> calendar
  outputIO config Theftcomm "Theftcomm Calendar Validation Errors" output

generate :: TheftcommConfig -> IO ()
generate = error "Not implemented"

summary :: TheftcommConfig -> IO ()
summary = error "Not implemented"
