module Day5
where

-- import Control.Lens
import Data.Char (toLower)
import Protolude hiding (head, min)

import Utils

import qualified Data.Text as T

day5 :: IO Text
day5 = do
  input1 <- readFile "input_day5_1.txt"
  pure $ (Utils.showT . length . process . T.unpack) input1

process :: [Char] -> [Char]
process xs = let 
  process' vals = runState (processOnce vals) (0, [])
  (_, (count, processed)) = process' xs
  in if count /= 0 then process processed else processed

-- This can be sped up if we scan by 4 values and collapse middle four. 
-- But the function might be ugly
processOnce :: [Char] -> State (Int, [Char]) [Char]
processOnce (x:y:xs) = do
  (count, values) <- get
  if isPolar x y
    then put (count + 1, values) >> processOnce xs
    else put (count, x:values) >> processOnce (y:xs)
  where
    isPolar :: Char -> Char -> Bool
    isPolar a b
      | a == b = False
      | toLower a == toLower b = True
      | otherwise = False

processOnce [x] = do
  (count, values) <- get
  put (count, reverse $ x:values)
  pure []

processOnce [] = panic "Empty list, dude!"

