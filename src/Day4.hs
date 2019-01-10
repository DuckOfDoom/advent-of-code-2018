module Day4
where

import Control.Lens
import Data.List    (head)
import Protolude    hiding (head, min)

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
  -- writeFile "out.txt" $ (mconcat . intersperse "\n" . map Utils.showT) (getLongestSleep filledRecords)
  writeFile "out.txt" $ Utils.showT (getLongestSleep filledRecords)
  pure ""

fillGuardIds :: [Record] -> State (Int, [Record]) ()
fillGuardIds (x:xs) = do
  (currId, result) <- get
  if x ^. guardId == 0
    then put (currId, (x & guardId .~ currId) : result)
    else put (x ^. guardId, x : result)
  fillGuardIds xs
fillGuardIds [] = modify (fmap reverse)

getLongestSleep :: [Record] -> (Int, [(Int, Int)], Int)
getLongestSleep xs =
  let
    toSeconds :: Record -> Int
    toSeconds r = (r ^. time . min * 60) + (r ^. time . sec)

    toRange :: Record -> Record -> [Int]
    toRange r1 r2 = [toSeconds r1 .. toSeconds r2]

    sleepStartTimes = filter (\r -> r ^. action == FallsAsleep) xs
    sleepStopTimes = filter (\r -> r ^. action == WakesUp) xs

    sleepTimes :: [(Int, [Int], Int)] -- (id, [ranges for sleep], total sleep time)
    sleepTimes = zipWith
      (\a b -> (a ^. guardId, toRange a b, toSeconds b - toSeconds a))
        sleepStartTimes
        sleepStopTimes

    sleepTimesByGuardId :: [(Int, [[Int]])] 
    sleepTimesByGuardId = HM.toList $ foldl (\hm (id, r, _) -> Utils.modifyOrAdd id (: r) [r] hm) HM.empty sleepTimes

    totalTimeByGuardId :: [(Int, Int)] 
    totalTimeByGuardId = foldl (\hm (id, (_, t)) -> Utils.modifyOrAdd id (+ t) t hm) HM.empty sleepTimes

    longestSleepingGuard = undefined -- maximumBy (\)
    in
      undefined
      -- map (\(id, time) -> )
