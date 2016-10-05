{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Mitsfs.Theftcomm.Iterate
  ( iterateTuple
  , iterate3Tuple, traceU
  ) where

import           Control.Applicative      ((<|>))
import           Control.Monad            (join)
import           Data.Maybe
import           Data.Time                (UTCTime, utc)
import qualified Data.Vector              as V

import           Mitsfs.Theftcomm.DoorLog
import           Text.ICalendar

import Mitsfs.Theftcomm.ICalendar
import           Debug.Trace

class (Show (a b), Show b) => IterableUTC a b where
  startUTC :: a b -> Maybe UTCTime
  endUTC   :: a b -> Maybe UTCTime
  next     :: a b -> Maybe (a b)
  item     :: a b -> Maybe b
  empty    :: a b
  traceU   :: a b -> a b
  nextDate :: UTCTime -> a b -> Maybe UTCTime
  nextDate startDt xs = join $ (\s -> if startDt < s  then Just s else endUTC xs) <$> startUTC xs
  itemD    :: UTCTime -> a b -> Maybe b
  itemD startDt xs = join $ (\s -> if startDt >= s then item xs else Nothing) <$> startUTC xs
  find     :: UTCTime -> a b -> a b
  find startDt xs = case endUTC xs of
      Nothing -> xs
      Just end | startDt >= end -> fromMaybe empty $ find startDt <$> next xs
               | otherwise      -> xs

boolMaybe :: Bool -> a -> Maybe a
boolMaybe b ys = if b then Nothing else Just ys

instance IterableUTC V.Vector DoorLog where
  startUTC xs = doorDate <$> item xs
  endUTC xs = join $ startUTC <$> next xs
  item xs = xs V.!? 0
  next xs = boolMaybe (null xs) (V.tail xs)
  empty = V.empty
  traceU = traceShowId

instance IterableUTC [] (RItem VEvent) where
  startUTC xs =  fromMaybe (error "Calendar entry must have start date") . rIStartDateUtc <$> item xs
  endUTC xs = fromMaybe (error "Calendar entry must have end date") . rIEndDateUtc <$> item xs
  item xs = boolMaybe (null xs) (head xs)
  next xs = boolMaybe (null xs) (tail xs)
  empty = []
  traceU xs = traceShow (showRItem (const utc) <$> xs) xs

iterateTuple :: (IterableUTC a b, IterableUTC c d) => UTCTime -> UTCTime -> a b -> c d -> [(UTCTime, UTCTime, Maybe b, Maybe d)]
iterateTuple st end l r = let
  rS = find st r
  lS = find st l
  nextD = minimum $ catMaybes [Just end, nextDate st rS, nextDate st lS]
  in if st == end then [] else
    (st, nextD, itemD st lS, itemD st rS) : iterateTuple nextD end lS rS

iterate3Tuple :: (IterableUTC a b, IterableUTC c d, IterableUTC e f) =>
  UTCTime -> UTCTime -> a b -> c d -> e f ->
  [(UTCTime, UTCTime, Maybe b, Maybe d, Maybe f)]
iterate3Tuple st end l m r = let
  rS = find st r
  mS = find st m
  lS = find st l
  nextD = minimum $ catMaybes [Just end, nextDate st rS, nextDate st mS, nextDate st lS]
  in if st == end then [] else
    (st, nextD, itemD st lS, itemD st mS, itemD st rS) : iterate3Tuple nextD end lS mS rS
