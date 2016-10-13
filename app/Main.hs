module Main where

import           Control.Monad       (join)
import           Data.Time           (Day, addDays, defaultTimeLocale,
                                      getCurrentTime, getCurrentTimeZone,
                                      localDay, parseTimeM, utcToLocalTime)
import           Options.Applicative

import qualified Mitsfs.Theftcomm    as Theftcomm

parseDay :: ReadM Day
parseDay = let
  f ws = maybe (Left ("Unable to parse date '" ++ ws ++ "' must be in format YYYY-MM-DD"))
    Right $ parseTimeM True defaultTimeLocale "%F" ws
  in eitherReader f

theftcommOptions :: Day -> Parser Theftcomm.TheftcommConfig
theftcommOptions today = Theftcomm.TheftcommConfig <$>
  option parseDay (long "date" <> short 'd' <> metavar "DATE" <> help "date to validate on" <> value today ) <*>
  strOption (long "door" <> metavar "DOOR_PATH" <> help "path to door.log file" <> value "/mit/mitsfs/ookcomm/door-sensor/logs/w20-473.log") <*>
  strOption (long "keyholders" <> metavar "KEYHOLDERS_PATH" <> help "path to Keyholder.json file" <> value "/mit/mitsfs/tif/keyholders.json") <*>
  strOption (long "ical" <> metavar "ICAL_PATH" <> help "path to ICalendar Folder" <> value "/mit/mitsfs/tif/calendar_logs") <*>
  strOption (long "hours" <> metavar "HOURS_PATH" <> help "path to daily hour files" <> value "/mit/mitsfs/tif/daily_hour_data") <*>
  strOption (long "from" <> metavar "FROM_EMAIL" <> help "From Email" <> value "theftcomm@mit.edu") <*>
  strOption (long "theftcomm-email" <> metavar "THEFTCOMM_EMAIL" <> help "Theftcomm Email" <> value "theftcomm@mit.edu") <*>
  strOption (long "keyholders-email" <> metavar "KEYHOLDERS_EMAIL" <> help "Keyholder Email" <> value "keyholders@mit.edu") <*>
  strOption (long "star-chamber-email" <> metavar "STAR_CHAMBER_EMAIL" <> help "Star Chamber Email" <> value "theftchamber@mit.edu") <*>
  strOption (long "host" <> metavar "HOST" <> help "Smtp Server" <> value "localhost") <*>
  switch (long "email" <> help "Should email output rather than print")

validate :: Day -> Parser (IO ())
validate day = Theftcomm.validate <$> theftcommOptions day

canceled :: Day -> Parser (IO ())
canceled day = Theftcomm.canceled <$> theftcommOptions day

generate :: Day -> Parser (IO ())
generate day = Theftcomm.generate <$> theftcommOptions day

summary :: Day -> Parser (IO ())
summary day = Theftcomm.summary <$> theftcommOptions day

validateDesc, canceledDesc, generateDesc, summaryDesc :: String
validateDesc = "Validates iCalendar data for MITSFS formatting"
canceledDesc = "Generate emails about canceled or improperly canceled hours"
generateDesc = "Generates keyholder stats for a date"
summaryDesc = "Generates keyholder stats summary for a date range"

opts :: Day -> Parser (IO ())
opts day = hsubparser
  (  command "validate" (info (validate day) ( progDesc validateDesc))
  <> command "canceled" (info (canceled day) ( progDesc canceledDesc))
  <> command "generate" (info (generate day) ( progDesc generateDesc))
  <> command "summary"  (info (summary day)  ( progDesc summaryDesc ))
  )
main :: IO ()
main = do
  tz <- getCurrentTimeZone
  today <- addDays (-1) . localDay . utcToLocalTime tz <$> getCurrentTime
  join $ customExecParser (prefs showHelpOnError) (info (helper <*> opts today) ( fullDesc
     <> progDesc "Theftcomm tyrannical iron fist utilities"
     <> header "theftcomm - utilities" ))
