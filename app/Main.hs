module Main where

import           Control.Monad       (join)
import           Data.Char           (isDigit)
import           Data.List           (span)
import           Data.Time           (Day, fromGregorianValid, getCurrentTime,
                                      getCurrentTimeZone, localDay,
                                      utcToLocalTime)
import           Options.Applicative

import qualified Mitsfs.Theftcomm    as Theftcomm

parseDay :: ReadM Day
parseDay = eitherReader f
 where
   dateError d = "Unable to parse date '" ++ d ++ "' must be in format YYYY-MM-DD"
   takeDigits xs = do
     (ds, ys) <- pure $ span isDigit (dropWhile (== '-') xs)
     if null ds then Left (dateError xs) else Right ()
     pure (read ds, ys)
   f ws = do
     (year, xs) <- takeDigits ws
     (month, ys) <- takeDigits xs
     (day, zs) <- takeDigits ys
     if null zs then Right () else Left (dateError ws)
     maybe (Left (dateError ws)) Right $ fromGregorianValid year month day

theftcommOptions :: Day -> Parser Theftcomm.TheftcommConfig
theftcommOptions today = Theftcomm.TheftcommConfig <$>
  option parseDay (long "date" <> short 'd' <> metavar "DATE" <> help "date to validate on" <> value today ) <*>
  strOption (long "keyholders" <> metavar "KEYHOLDERS_PATH" <> help "path to Keyholder.json file" <> value "/mit/mitsfs/tif/keyholders.json") <*>
  strOption (long "ical" <> metavar "ICAL_PATH" <> help "path to ICalendar Folder" <> value "/mit/mitsfs/tif/calendar_logs") <*>
  strOption (long "hours" <> metavar "HOURS_PATH" <> help "path to daily hour files" <> value "/mit/mitsfs/tif/daily_hour_data") <*>
  strOption (long "from" <> metavar "FROM_EMAIL" <> help "From Email" <> value "theftcomm@mit.edu") <*>
  strOption (long "theftcomm-email" <> metavar "THEFTCOMM_EMAIL" <> help "Theftcomm Email" <> value "theftcomm@mit.edu") <*>
  strOption (long "keyholders-email" <> metavar "KEYHOLDERS_EMAIL" <> help "Keyholder Email" <> value "keyholders@mit.edu") <*>
  strOption (long "star-chamber-email" <> metavar "STAR_CHAMBER_EMAIL" <> help "Star Chamber Email" <> value "star-chamber@mit.edu") <*>
  switch (long "email" <> help "Should email output rather than print")

validate :: Day -> Parser (IO ())
validate day = Theftcomm.validate <$> theftcommOptions day

generate :: Day -> Parser (IO ())
generate day = Theftcomm.generate <$> theftcommOptions day

summary :: Day -> Parser (IO ())
summary day = Theftcomm.summary <$> theftcommOptions day

validateDesc, generateDesc, summaryDesc :: String
validateDesc = "Validates iCalendar data for MITSFS formatting"
generateDesc = "Generates keyholder stats for a date"
summaryDesc = "Generates keyholder stats summary for a date range"

opts :: Day -> Parser (IO ())
opts day = hsubparser
  (  command "validate" (info (validate day) ( progDesc validateDesc))
  <> command "generate" (info (generate day) ( progDesc generateDesc))
  <> command "summary"  (info (summary day)  ( progDesc summaryDesc ))
  )
main :: IO ()
main = do
  tz <- getCurrentTimeZone
  today <- localDay . utcToLocalTime tz <$> getCurrentTime
  join $ customExecParser (prefs showHelpOnError) (info (helper <*> opts today) ( fullDesc
     <> progDesc "Theftcomm tyrannical iron fist utilities"
     <> header "theftcomm - utilities" ))
