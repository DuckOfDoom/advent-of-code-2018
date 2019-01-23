module Utils where

import Protolude
-- import GHC.Show
import System.CPUTime (getCPUTime)

import qualified Data.Text as T

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

showT :: (Show a) => a -> Text
showT = T.pack . show

modifyOrAdd ::(Eq k, Hashable k) => k -> (v -> v) -> v -> HashMap k v -> HashMap k v
modifyOrAdd key func value hmap
  | HM.member key hmap = HM.adjust func key hmap
  | otherwise = HM.insert key value hmap

-- makes pairs of all non-equal elements in a list.
-- e.g. mkParis "abc" = [('a','b'),('a','c'),('b','a'),('b','c'),('c','a'),('c','b')]
mkPairsSelf :: (Eq a) => [a] -> [(a, a)]
mkPairsSelf l = concatMap (\e -> mkPairs' e l) l
  where
    mkPairs' :: (Eq a) => a -> [a] -> [(a,a)]
    mkPairs' k (y:ys) =
      if k == y
        then
          mkPairs' k ys
        else
          (k, y) : mkPairs' k ys
    mkPairs' _ [] = []

-- same as above but with two different lists and with duplicates
mkPairs :: (Eq a) => [a] -> [a] -> [(a, a)]
mkPairs x = concatMap (\e -> mkPairs' e x)
  where
    mkPairs' :: (Eq a) => a -> [a] -> [(a,a)]
    mkPairs' k (y:ys) = (k, y) : mkPairs' k ys
    mkPairs' _ []     = []

-- groups entries by key using hashmap
groupBy :: (Eq k, Hashable k) => (a -> k) -> [a] -> [[a]]
groupBy selector xs =
  let m = foldl (\hm v -> modifyOrAdd (selector v) (v :) [v] hm) (HM.empty :: HashMap k [a]) xs
  in
    map snd . HM.toList $ m

countOccurences :: (Eq a, Hashable a) => [a] -> HashMap a Int
countOccurences = foldl (\hm' x -> modifyOrAdd x (+1) 1 hm') HM.empty

countOccurences2 :: (Eq a, Hashable a) => [[a]] -> HashMap a  Int
countOccurences2 = foldl (\hm xs -> HM.unionWith (+) hm (countOccurences xs)) HM.empty

maxBy :: (Ord b) => (a -> b) -> [a] -> a
maxBy selector = maximumBy (\a1 a2 -> selector a1 `compare` selector a2)

executeWithTimer :: Text -> IO t -> IO t
executeWithTimer name f = do
  start <- getCPUTime
  v <- f >>= evaluate
  end <- getCPUTime
  let
    timeDiff = fromIntegral (end - start) / (10^(12 :: Integer))
  putStrLn $ mconcat ["Job \"", name, "\" was completed in ",  (showT (timeDiff :: Double)), " seconds"]
  pure v
