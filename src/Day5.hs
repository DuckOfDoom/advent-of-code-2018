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
  writeFile "out.txt" $ (T.pack . fullyReact . T.unpack) input1
  _ <- Utils.executeWithTimer "process" $ (pure . length . process . T.unpack) input1
  var2 <- Utils.executeWithTimer "fullyReact" $ (pure . length . fullyReact . T.unpack) input1
  pure $ (Utils.showT var2)

fullyReact :: [Char] -> [Char]
fullyReact xs = react (length xs) xs
  where
    react prevLength ys =
      let
        current = process2 ys
        currLength = length current
        in
        -- trace (Utils.showT prevLength) $
        if currLength == prevLength
          then current
          else react currLength current

test = Utils.executeWithTimer "derp" $ (pure . fullyReact) "dabAcCaCBAcCcaDA"

process :: [Char] -> [Char]
process xs = let
  process' vals = runState (processOnce vals) (0, [])
  (_, (count, processed)) = process' xs
  in if count /= 0 then process processed else processed

-- This can be sped up if we scan by 4 values and collapse middle two.
-- But the function might be ugly
processOnce :: [Char] -> State (Int, [Char]) [Char]
processOnce (x:y:xs) = do
  (count, values) <- get
  if isPolar x y
    then put (count + 1, values) >> processOnce xs
    else put (count, x:values) >> processOnce (y:xs)
processOnce [x] = do
  (count, values) <- get
  put (count, reverse $ x:values)
  pure []

processOnce [] = panic "Empty list, dude!"

process2 :: [Char] -> [Char]
process2 (x1:x2:x3:xs)
  | isPolar x1 x2 = process2 (x3:xs)
  | isPolar x2 x3 = process2 (x1:xs)
  | otherwise = x1 : process2 (x2:x3:xs)

process2 (x1:x2:xs)
  | isPolar x1 x2 = process2 xs
  | otherwise = x1:x2:process2 xs

process2 [x] = [x]
process2 [] = []

isPolar :: Char -> Char -> Bool
isPolar a b
  | a == b = False
  | toLower a == toLower b = True
  | otherwise = False
