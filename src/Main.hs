module Main where

import Protolude
import Day1  

import Data.Text as T

main :: IO ()
main = do
  day1 >>= printResult 1

printResult :: Int -> Text -> IO ()
printResult day result = print $ "Day " <> (T.pack . show) day <> " " <> result