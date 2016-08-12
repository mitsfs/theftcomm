module Main where

import Options.Applicative
import Control.Monad

import Lib

validate' :: String -> IO ()
validate' = putStrLn
generate' :: String -> IO ()
generate' = error "Not implemented"
summary' :: String -> IO ()
summary' = error "Not implemented"

validate :: Parser (IO ())
validate = validate' <$> argument str idm

generate :: Parser ( IO () )
generate = generate' <$> argument str idm

summary :: Parser ( IO () )
summary = summary' <$> argument str idm

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
