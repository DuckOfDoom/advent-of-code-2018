module Main where

import Protolude

import Day1
import Day2
import Day3

import Data.Text as T

main :: IO ()
main = do
  day1 >>= printResult 1
  day2 >>= printResult 2
  day3 >>= printResult 3

printResult :: Int -> Text -> IO ()
printResult day result = print $ "Day " <> (T.pack . show) day <> " results: " <> result
