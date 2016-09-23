module Main where

import           Control.Monad       (join)
import           Options.Applicative

import qualified Mitsfs.Theftcomm    as Theftcomm

validate :: Parser (IO ())
validate = Theftcomm.validate <$> argument str idm

generate :: Parser (IO ())
generate = Theftcomm.generate <$> argument str idm

summary :: Parser (IO ())
summary = Theftcomm.summary <$> argument str idm

validateDesc, generateDesc, summaryDesc :: String
validateDesc = "Validates iCalendar data for MITSFS formatting"
generateDesc = "Generates keyholder stats for a date"
summaryDesc = "Generates keyholder stats summary for a date range"

opts :: Parser (IO ())
opts = subparser
  (  command "validate" (info validate ( progDesc validateDesc))
  <> command "generate" (info generate ( progDesc generateDesc))
  <> command "summary"  (info summary  ( progDesc summaryDesc ))
  )
main :: IO ()
main = join $ execParser (info opts ( fullDesc
     <> progDesc "Theftcomm tyrannical iron fist utilities"
     <> header "theftcomm - utilities" ))
