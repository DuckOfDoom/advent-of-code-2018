module Day5
where

-- import Control.Lens
import Data.Char (toLower)
-- import Data.List (elemIndex)
import Protolude hiding (head, min)
import Control.Parallel.Strategies (parMap, rdeepseq)

import qualified Prelude (readFile)
import qualified Data.Char as Ch

import Utils

day5 :: IO Text
day5 = do
  input <- Prelude.readFile "input_day5_1.txt"
  a1 <- answer1 input
  a2 <- answer2 input
  pure $ (mconcat . intersperse " " . map Utils.showT) [a1, a2]

-- wordToNumbers :: [Char] -> [Char]
-- wordToNumbers = map letterToNumber 
--   where 
--     letterToNumber :: Char -> Char
--     letterToNumber ch = show $ fromMaybe 0 $ elemIndex ch ['a'..'z']

answer1 :: [Char] -> IO Int
answer1 input = Utils.executeWithTimer "Day5, answer1" $ (pure . length . fullyReact) input

answer2 :: [Char] -> IO Int
answer2 input = 
  let 
    results :: [[Char]] 
    results = parMap rdeepseq (\ch -> fullyReactWithout ch input) ['A'..'Z']
    in
    Utils.executeWithTimer "Day5, answer2" $
      (pure . length . minimumBy (\a b -> length a `compare` length b)) results

fullyReactWithout :: Char -> [Char] -> [Char]
fullyReactWithout toRemove xs = 
  fullyReact $ filter (\c -> c /= Ch.toUpper toRemove && c /= Ch.toLower toRemove) xs

------------------------------------------------------------------------------------------------

fullyReact :: [Char] -> [Char]
fullyReact xs = react (length xs) xs
  where
    react prevLength ys =
      let
        current = process ys
        currLength = length current
        in
        -- trace (Utils.showT prevLength) $
        if currLength == prevLength
          then current
          else react currLength current

------------------------------------------------------------------------------------------------

process :: [Char] -> [Char]
process (x1:x2:x3:xs)
  | isPolar x1 x2 = process (x3:xs)
  | isPolar x2 x3 = process (x1:xs)
  | otherwise = x1 : process (x2:x3:xs)

process (x1:x2:xs)
  | isPolar x1 x2 = process xs
  | otherwise = x1:x2:process xs

process [x] = [x]
process [] = []

isPolar :: Char -> Char -> Bool
isPolar a b
  | a == b = False
  | toLower a == toLower b = True
  | otherwise = False