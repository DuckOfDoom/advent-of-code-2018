{-# LANGUAGE DeriveGeneric #-}

module Day2
-- ( day2 )
where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T 
import           Protolude
import           Utils

data Count = None | Two | Three | Both
  deriving (Show, Eq, Generic)

instance Hashable Count

day2 :: IO Text
day2 = do
  input1 <- T.lines <$> readFile "input_day2_1.txt"
  putStrLn $ showT $ calcWordsCount input1 
  pure $ showT $ (calcChecksum . calcWordsCount) input1

calcChecksum :: HashMap Count Int -> Int
calcChecksum hm =
  let
    getCount = HM.lookupDefault 0 
    twosCount = getCount Two hm + getCount Both hm
    threesCount = getCount Three hm + getCount Both hm
  in
    twosCount * threesCount

calcWordsCount :: [Text] -> HashMap Count Int
calcWordsCount = foldl (\hm t -> (modifyOrAdd (calcCount t) (+1) 1 hm)) HM.empty

calcCount :: Text -> Count
calcCount x
  | hasTwos && hasThrees = Both
  | hasTwos = Two
  | hasThrees = Three
  | otherwise = None
  where
    hasTwos = any (\(_, c) -> c == 2) countsMap
    hasThrees = any (\(_, c) -> c == 3) countsMap

    countsMap = HM.toList (fillMap (T.unpack x))

    fillMap :: [Char] -> HashMap Char Int
    fillMap = foldl (\hm ch -> (modifyOrAdd ch (+1) 1 hm)) HM.empty 