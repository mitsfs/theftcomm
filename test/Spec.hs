module Main where
import Test.Hspec

import Data.Time (Day, fromGregorian)

import Mitsfs.Theftcomm
import Paths_theftcomm


getConfig :: String -> Day -> TheftcommConfig
getConfig p d = TheftcommConfig d (p ++ "/w20-473.log") (p ++ "/keyholders.json") (p ++ "/calendar_logs") (p ++ "/daily_hour_data") e e e e "localhost" False
  where e = "theftcomm@example.com"

main :: IO ()
main = do
  path <- getDataDir
  let config = getConfig (path ++ "/test/data") (fromGregorian 2016 10 7)
  hspec $ do
    describe "Validate" $ do
      it "should run" $ do
        validate config
    describe "Canceled" $ do
      it "should run" $ do
        canceled config
    describe "Generate" $ do
      it "should run" $ do
        generate config
    describe "Summary" $ do
      it "should run" $ do
        summary config
