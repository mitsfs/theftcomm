{-# LANGUAGE OverloadedStrings #-}
module Mitsfs.Theftcomm
  ( validate
  , generate
  , summary
  ) where

import qualified Codec.Compression.GZip     as GZip
import qualified Data.ByteString.Lazy       as BS
import           Data.List
import           Data.Time                  (fromGregorian)

import           Text.ICalendar

import           Mitsfs.Theftcomm.ICalendar

readFileMGzip :: String -> IO BS.ByteString
readFileMGzip path = (if ".gz" `isSuffixOf` path then GZip.decompress else id)
                       <$> BS.readFile path

validate :: String -> IO ()
validate path = do
  content <- readFileMGzip path
  print $ fmap rIStartDateUtc <$> getICalEventsDay content (fromGregorian 2016 2 10)

generate :: String -> IO ()
generate = error "Not implemented"

summary  :: String -> IO ()
summary = error "Not implemented"
