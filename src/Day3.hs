{-# LANGUAGE DeriveGeneric #-}

module Day3
( day3 )
where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS

import qualified Data.Text as T
import qualified Utils

import Control.Lens
import Prelude      (read)
import Protolude

data Claim = Claim
  { _id      :: Int
  , _offsetX :: Int
  , _offsetY :: Int
  , _sizeX   :: Int
  , _sizeY   :: Int
  }
  deriving (Show, Eq, Generic)
makeLenses ''Claim

instance Hashable Claim

day3 :: IO Text
day3 = do
  input1 <- T.lines <$> readFile "input_day3_1.txt"
  let
    claims = map parseClaim input1
    (_, (notOverlappedPoints, pointsWithCountsMap)) =
      runState
       (findIntersectingPoints claims)
       (claims, HM.empty)
    answer1 = (Utils.showT . length . filter (\(_, count) -> count >= 2)) (HM.toList pointsWithCountsMap)
    answer2 = (Utils.showT . fmap (^. id) . head) notOverlappedPoints

  pure $ mconcat [answer1, ", ", answer2]

type Point = (Int, Int)
type PointsMap = HashMap Point Int

findIntersectingPoints :: [Claim] -> State ([Claim], PointsMap) ()
findIntersectingPoints (x:xs) = do
  (l, m) <- get
  let
    newM = addPointsToMap m x
    in
    put (l, newM)
  findIntersectingPoints xs
    where
      addPointsToMap :: PointsMap -> Claim -> PointsMap
      addPointsToMap hm c = foldl (\h p -> Utils.modifyOrAdd p (+1) 1 h) hm (getPoints c)

-- after we fill the map, remove all claims from list that have points with 2 or more intersections
findIntersectingPoints [] = do
  (l, m) <- get
  let
    pointsWithoutIntersections = HS.fromList $ map fst (filter (\(_, count) -> count <= 1) (HM.toList m))
    allPointsDontOverlap :: Claim -> Bool
    allPointsDontOverlap claim = 
      let points = getPoints claim
      in
         all (\p -> HS.member p pointsWithoutIntersections) points
    newL = filter allPointsDontOverlap l
    in
    put (newL, m)

-- #1 @ 1,3: 4x4
parseClaim :: Text -> Claim
parseClaim t =
  let
    str = T.unpack t
    notAt = (/= '@')
    notComma = (/= ',')
    notColon = (/= ':')
    notX = (/= 'x')

    _id = read ((takeWhile notAt . drop 1) str)
    _offsetX = read ((takeWhile notComma . drop 1 . dropWhile notAt) str)
    _offsetY = read ((takeWhile notColon . drop 1 . dropWhile notComma) str)
    _sizeX = read ((takeWhile notX . drop 1 . dropWhile notColon) str)
    _sizeY = read ((drop 1 . dropWhile notX) str)
  in
    Claim{..}

-- get all points in a claimed piece
getPoints :: Claim -> [(Int, Int)]
getPoints c = let
  startX = c ^. offsetX + 1
  endX = c ^. offsetX  + c ^. sizeX
  startY = c ^. offsetY + 1
  endY = c ^. offsetY + c ^. sizeY
  in Utils.mkPairs
    [startX .. endX]
    [startY .. endY]
