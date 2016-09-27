module Main where

import           Control.Monad       (join)
import           Data.Char           (isDigit)
import           Data.List           (span)
import           Data.Time           (Day, fromGregorian)
import           Options.Applicative

import qualified Mitsfs.Theftcomm    as Theftcomm

parseDay :: ReadM Day
parseDay = eitherReader f
 where
   f s = let
     (year, xs) = span isDigit s
     (month, ys) = span isDigit (tail xs)
     day = tail ys
     in Right $ fromGregorian (read year) (read month) (read day)

validate :: Parser (IO ())
validate = Theftcomm.validate <$>
  optional (option auto (long "date" <> short 'd' <> metavar "Date" <> help "date to validate on" )) <*>
  argument str (metavar "KEYHOLDERS" <> help "path to Keyholder.json file") <*>
  argument str (metavar "ICAL" <> help "path to ICalendar File")

generate :: Parser (IO ())
generate = Theftcomm.generate <$> argument str idm

summary :: Parser (IO ())
summary = Theftcomm.summary <$> argument str idm

validateDesc, generateDesc, summaryDesc :: String
validateDesc = "Validates iCalendar data for MITSFS formatting"
generateDesc = "Generates keyholder stats for a date"
summaryDesc = "Generates keyholder stats summary for a date range"

opts :: Parser (IO ())
opts = hsubparser
  (  command "validate" (info validate ( progDesc validateDesc))
  <> command "generate" (info generate ( progDesc generateDesc))
  <> command "summary"  (info summary  ( progDesc summaryDesc ))
  )
main :: IO ()
main = join $ customExecParser (prefs showHelpOnError) (info (helper <*> opts) ( fullDesc
     <> progDesc "Theftcomm tyrannical iron fist utilities"
     <> header "theftcomm - utilities" ))
