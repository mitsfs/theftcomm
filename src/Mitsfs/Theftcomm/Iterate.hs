{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}


module Mitsfs.Theftcomm.Iterate
  (
  ) where

import           Control.Applicative      ((<|>))
import           Control.Monad            (join)

import           Data.Maybe
import           Data.Time                (UTCTime)
import qualified Data.Vector              as V


import           Text.ICalendar

import           Mitsfs.Theftcomm.DoorLog

class IterableUTC a b where
  startUTC :: a b -> Maybe UTCTime
  endUTC   :: a b -> Maybe UTCTime
  next     :: a b -> Maybe (a b)
  item     :: a b -> Maybe b
  empty    :: a b
  nextDate :: UTCTime -> a b -> Maybe UTCTime
  nextDate dt xs = join $ (\s -> if dt >= s then endUTC xs else Just s) <$> startUTC xs
  itemD    :: UTCTime -> a b -> Maybe b
  itemD dt xs = join $ (\s -> if dt >= s then item xs else Nothing) <$> startUTC xs
  find     :: UTCTime -> a b -> a b
  find dt xs = let end = endUTC xs
    in case end of
      Nothing -> xs
      Just e  -> if e > dt then xs else fromMaybe empty $ find dt <$> next xs

boolMaybe :: Bool -> a -> Maybe a
boolMaybe b ys = if b then Nothing else Just ys

instance IterableUTC V.Vector DoorLog where
  startUTC xs = doorDate <$> item xs
  endUTC xs = join $ startUTC <$> next xs
  item xs = xs V.!? 0
  next xs = boolMaybe (null xs) (V.tail xs)
  empty = V.empty

instance IterableUTC [] (RItem VEvent) where
  startUTC xs =  fromMaybe (error "Calendar entry must have start date") . rIStartDateUtc <$> item xs
  endUTC xs = fromMaybe (error "Calendar entry must have end date") . rIStartDateUtc <$> item xs
  item xs = boolMaybe (null xs) (head xs)
  next xs = boolMaybe (null xs) (tail xs)
  empty = []

iterateTupple :: (IterableUTC a b, IterableUTC c d) => UTCTime -> UTCTime -> a b -> c d -> [(UTCTime, UTCTime, Maybe b, Maybe d)]
iterateTupple st end l r = let
  rS = find st r
  lS = find st l
  nextD = minimum $ catMaybes [Just end, nextDate st rS, nextDate st lS]
  in (st, nextD, itemD st lS, itemD st rS) : iterateTupple nextD end lS rS

iterate3Tupple :: (IterableUTC a b, IterableUTC c d, IterableUTC e f) =>
  UTCTime -> UTCTime -> a b -> c d -> e f ->
  [(UTCTime, UTCTime, Maybe b, Maybe d, Maybe f)]
iterate3Tupple st end l m r = let
  rS = find st r
  mS = find st m
  lS = find st l
  nextD = minimum $ catMaybes [Just end, nextDate st rS, nextDate st mS, nextDate st lS]
  in (st, nextD, itemD st lS, itemD st mS, itemD st rS) : iterate3Tupple nextD end lS mS rS
