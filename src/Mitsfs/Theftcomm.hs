{-# LANGUAGE OverloadedStrings #-}
module Mitsfs.Theftcomm
  ( validate
  , canceled
  , generate
  , summary
  , TheftcommConfig(..)
  ) where

import qualified Codec.Compression.GZip     as GZip
import           Control.Lens               ((&), (+~), (-~))
import           Control.Monad              (join)
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy       as BS
import           Data.Csv                   (decodeByName,
                                             encodeDefaultOrderedByName)
import           Data.List
import qualified Data.Map                   as M
import           Data.Maybe
import           Data.Text.Lazy             (Text, pack)
import           Data.Time                  (Day, defaultTimeLocale, formatTime)
import           Data.Time.Lens             (days, flexD, months)
import qualified Data.Vector                as V
import           Network.HaskellNet.SMTP

import           Mitsfs.Theftcomm.Config
import           Mitsfs.Theftcomm.DoorLog
import           Mitsfs.Theftcomm.ICalendar
import           Mitsfs.Theftcomm.Tif
import           Mitsfs.Theftcomm.Validate


outputIO :: TheftcommConfig -> TheftcommDest -> String -> String -> IO ()
outputIO config dest subject xs
  | null xs = pure ()
  | tcShouldEmail config = let
      server       = tcEmailHost config
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

dataFileName :: Day -> String
dataFileName = formatTime defaultTimeLocale "/raw/tif-raw-data-%F.csv"

summaryFileName :: Day -> String
summaryFileName = formatTime defaultTimeLocale "/tif-summary-%F.csv"

getICalFile :: TheftcommConfig -> Day -> IO BS.ByteString
getICalFile config d = readFileMGzip (tcICalendarFolder config ++ icalFileName d)

getSummaryFile :: TheftcommConfig -> Day -> IO [TifEntry TifLog]
getSummaryFile config d = do
  contents <- BS.readFile (tcTheftcommDataFolder config ++ dataFileName d)
  either error (pure . V.toList . snd) $ decodeByName contents

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


canceled :: TheftcommConfig -> IO ()
canceled config = do
  c <- mapM (allCanceledHours config) [0..6]
  outputIO config Keyholders "MITSFS Canceled Hours" (intercalate "\n\n" $ join c)

validate :: TheftcommConfig -> IO ()
validate config = do
  content <- getICalFile config $ tcDate config
  keyholders <- decodeKeyholders <$> BS.readFile (tcKeyholderPath config)
  let startDay = tcDate config & flexD.days -~ numDaysBack
  let calendar = getICalEventsDays content startDay numDays
  let tzf = getTZOffset content
  let output = either id (intercalate "\n\n") $ allFormattedErrors tzf keyholders <$> calendar
  outputIO config Theftcomm "Theftcomm Calendar Validation Errors" output

generate :: TheftcommConfig -> IO ()
generate config = do
  let today = tcDate config
      start = today & flexD.days -~ 7
  content <- getICalFile config today
  newContent <- getICalFile config start
  let calendar = either error id $ getICalEventsDays content today 1
      newCalendar = either error id $ getICalEventsDays newContent today 1
      tz = getTZOffset content today
  doorLog <- getDoorLog config
  let tifEntry = generateHours tz (toUTC today tz) newCalendar calendar doorLog
      csv = encodeDefaultOrderedByName tifEntry
      path = tcTheftcommDataFolder config ++ dataFileName today
  BS.writeFile path csv

summary :: TheftcommConfig -> IO ()
summary config = do
  let today = tcDate config
      test = "dfdf"
      start = today & flexD.months -~ 1
  entries <- mapM (getSummaryFile config) [start .. today]
  let csv = encodeDefaultOrderedByName $ summaryHours (join entries)
      path = tcTheftcommDataFolder config ++ summaryFileName today
      output = "Summary data is located on AFS at " ++ show path
  BS.writeFile path csv
  outputIO config StarChamber "Theftcomm Summary Data Generated" output
