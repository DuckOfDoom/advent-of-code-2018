module Main where

import Protolude

import Day1
import Day2
import Day3
import Day4
import Day5

import Data.Text as T

main :: IO ()
main = do
  say "Calculating day 1..."
  day1 >>= printResult 1
  say "Calculating day 2..."
  day2 >>= printResult 2
  say "Calculating day 3..."
  day3 >>= printResult 3
  say "Calculating day 4..."
  day4 >>= printResult 4
  say "Calculating day 5..."
  day5 >>= printResult 5

printResult :: Int -> Text -> IO ()
printResult day result = print $ "Day " <> (T.pack . show) day <> " results: " <> result

say :: Text -> IO ()
say = print
