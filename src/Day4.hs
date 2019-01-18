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
    ((g1, s1), (g2, s2)) = getAnswers filledRecords
    ans1 = g1 * s1
    ans2 = g2 * s2
  pure $ mconcat [Utils.showT ans1, ", ", Utils.showT ans2]

fillGuardIds :: [Record] -> State (Int, [Record]) ()
fillGuardIds (x:xs) = do
  (currId, result) <- get
  if x ^. guardId == 0
    then put (currId, (x & guardId .~ currId) : result)
    else put (x ^. guardId, x : result)
  fillGuardIds xs
fillGuardIds [] = modify (fmap reverse)

getAnswers :: [Record] -> ((Int, Int), (Int, Int))
getAnswers xs =
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

    guardIdToMinutesAsleep :: [(Int, [Int])]
    guardIdToMinutesAsleep = HM.toList $ foldl (\hm (id, r, _) -> Utils.modifyOrAdd id (++ r) r hm) HM.empty sleepTimes

    totalTimeByGuardId :: [(Int, Int)] 
    totalTimeByGuardId = HM.toList $ foldl (\hm (id, _, t) -> Utils.modifyOrAdd id (+ t) t hm) HM.empty sleepTimes
 
    longestSleepingGuard :: (Int, Int)
    longestSleepingGuard = Utils.maxBy snd totalTimeByGuardId

    sleepyMinute :: Int -- The most sleepy minute for a most sleeping guard
    sleepyMinute = let 
      ranges = sleepRangesByGuardId ! (longestSleepingGuard ^. _1)
      minuteCounts = (HM.toList . Utils.countOccurences2) ranges
      in 
        Utils.maxBy snd minuteCounts ^. _1

    answer1 :: (Int, Int)
    answer1 = (longestSleepingGuard ^. _1, sleepyMinute)

    answer2 :: (Int, Int)
    answer2 = let 
      guardToMostSleepyMinute :: (Int, [Int]) -> (Int, (Int, Int)) -- (guardId, (minute, count))
      guardToMostSleepyMinute (id, ys) = (id, Utils.maxBy snd $ HM.toList $ Utils.countOccurences ys)

      sleepiestGuardRecord :: (Int, (Int, Int))
      sleepiestGuardRecord = Utils.maxBy (snd . snd) (map guardToMostSleepyMinute guardIdToMinutesAsleep)    
      in 
        (sleepiestGuardRecord ^. _1, sleepiestGuardRecord ^. (_2 . _1))
    in
      (answer1, answer2)
