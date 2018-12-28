module Day1 
where

import Prelude (String, read)
import Protolude 
-- import Protolude hiding readFile
import qualified Data.Text as T

day1 :: IO Text
day1 = do
  inputList <- T.lines <$> readFile "input_day1.txt"
  pure (T.pack . show $ calculate inputList)
  where 
    calculate :: [Text] -> Int
    calculate = foldl (\res str -> (lineToFunc . T.unpack) str res) 0 

    lineToFunc :: String -> (Int -> Int)
    lineToFunc (s:xs) = case s of
      '+' -> (+ (read xs :: Int))
      '-' -> (subtract (read xs :: Int))
      _ -> identity

    lineToFunc [] = identity