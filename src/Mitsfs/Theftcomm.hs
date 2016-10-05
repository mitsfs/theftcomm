{-# LANGUAGE OverloadedStrings #-}
module Mitsfs.Theftcomm
  ( validate
  , generate
  , summary
  , TheftcommConfig(..)
  ) where

import qualified Codec.Compression.GZip     as GZip
import           Control.Lens               ((&), (+~), (-~))
import           Control.Monad              (join)
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

getICalFile :: TheftcommConfig -> Day -> IO BS.ByteString
getICalFile config d = readFileMGzip (tcICalendarFolder config ++ icalFileName d)

allCanceledHours :: TheftcommConfig -> Int -> IO [String]
allCanceledHours config offsetDay = do
  let today = tcDate config
  content <- getICalFile config today
  let start = today & flexD.days -~ (7 - offsetDay)
  let end = today & flexD.days +~ offsetDay
  newContent <- getICalFile config start
  let calendar = either error id $ getICalEventsDays content end 1
  let newCalendar = either error id $ getICalEventsDays newContent end 1
  let tz = getTZOffset content end
  pure $ canceledHours tz (toUTC end tz) newCalendar calendar


validate :: TheftcommConfig -> IO ()
validate config = do
  content <- getICalFile config $ tcDate config
  keyholders <- decodeKeyholders <$> BS.readFile (tcKeyholderPath config)
  let startDay = tcDate config & flexD.days -~ numDaysBack
  let calendar = getICalEventsDays content startDay numDays
  let tzf = getTZOffset content
  let output = either id (intercalate "\n\n") $ allFormattedErrors tzf keyholders <$> calendar
  outputIO config Theftcomm "Theftcomm Calendar Validation Errors" output
  canceled <- mapM (allCanceledHours config) [0..6]
  outputIO config Keyholders "MITSFS Canceled Hours" (intercalate "\n\n" $ join canceled)

generate :: TheftcommConfig -> IO ()
generate = error "Not implemented"

summary :: TheftcommConfig -> IO ()
summary = error "Not implemented"
