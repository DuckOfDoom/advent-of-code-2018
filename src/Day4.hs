module Day4
where

import Control.Lens
import Data.List    (head, init)
import Protolude    hiding (head, min)

import Data.HashMap.Strict (HashMap, (!))

import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T
import qualified Utils

import Day4.Types

day4 :: IO Text
day4 = do
  input1 <- T.lines <$> readFile "input_day4_1.txt"
  let
    sortedRecords = (sort . map parseRecord) input1
    (_, (_, filledRecords)) = runState (fillGuardIds sortedRecords) (head sortedRecords ^. guardId, [])
    guardWithSleepySeconds = getLongestSleep filledRecords
    result = guardWithSleepySeconds ^. _1 * guardWithSleepySeconds ^. _2
  print $ Utils.showT guardWithSleepySeconds
  -- print $ Utils.showT result
  pure $ Utils.showT result

fillGuardIds :: [Record] -> State (Int, [Record]) ()
fillGuardIds (x:xs) = do
  (currId, result) <- get
  if x ^. guardId == 0
    then put (currId, (x & guardId .~ currId) : result)
    else put (x ^. guardId, x : result)
  fillGuardIds xs
fillGuardIds [] = modify (fmap reverse)

getLongestSleep :: [Record] -> (Int, Int)
getLongestSleep xs =
  let
    toSeconds :: Record -> Int
    toSeconds r = (r ^. time . min * 60) + (r ^. time . sec)

    toRange :: Record -> Record -> [Int]
    toRange r1 r2 = init [toSeconds r1 .. toSeconds r2]

    sleepStartTimes = filter (\r -> r ^. action == FallsAsleep) xs
    sleepStopTimes = filter (\r -> r ^. action == WakesUp) xs

    -- zip times by guard id
    sleepTimes :: [(Int, [Int], Int)] -- (id, [ranges for sleep], total sleep time)
    sleepTimes = zipWith
      (\a b -> (a ^. guardId, toRange a b, toSeconds b - toSeconds a))
        sleepStartTimes
        sleepStopTimes

    sleepRangesByGuardId :: HashMap Int [[Int]]
    sleepRangesByGuardId = foldl (\hm (id, r, _) -> Utils.modifyOrAdd id (r :) [r] hm) HM.empty sleepTimes

    totalTimeByGuardId :: [(Int, Int)] 
    totalTimeByGuardId = HM.toList $ foldl (\hm (id, _, t) -> Utils.modifyOrAdd id (+ t) t hm) HM.empty sleepTimes
 
    longestSleepingGuard :: (Int, Int)
    longestSleepingGuard = Utils.maxBy snd totalTimeByGuardId

    sleepyMinute :: Int
    sleepyMinute = let 
      ranges = sleepRangesByGuardId ! (longestSleepingGuard ^. _1)
      minuteCounts = Utils.countOccurences ranges
      in 
        Utils.maxBy snd minuteCounts ^. _1
    in
      (longestSleepingGuard ^. _1, sleepyMinute)
