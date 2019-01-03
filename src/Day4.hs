module Day4
where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS

import           Data.Char (isDigit)
import qualified Data.Text as T
import qualified Utils

import Control.Lens
import Prelude      (read)
import Protolude    hiding (min)

data Action = Begins | FallsAsleep | WakesUp
  deriving (Show, Eq, Ord)

-- I don't like 'time' library, so i implement my own types for these records
-- Time ------------------------------------------
data Time = Time { _min :: Int, _sec :: Int}
  deriving (Show, Eq)
makeLenses ''Time

instance Ord Time where
  t1 `compare` t2
    | (t1 ^. min) == (t2 ^. min) = (t1 ^. sec) <=> (t2 ^. sec)
    | otherwise = (t1 ^. min) <=> (t2 ^. min)
    where (<=>) = compare

-- Date ------------------------------------------

data Date = Date { _year :: Int, _month :: Int, _day :: Int }
  deriving (Show, Eq)
makeLenses ''Date

instance Ord Date where
  d1 `compare` d2
    | d1 ^. year /= d2 ^. year = (d1 ^. year) <=> (d2 ^. year)
    | d1 ^. month /= d2 ^. month = (d1 ^. month) <=> (d2 ^. month)
    | otherwise = (d1 ^. day) <=> (d2 ^. day)
    where (<=>) = compare
-- ------------------------------------------

data Record = Record
  { _guardId :: Int
  , _date    :: Date
  , _time    :: Time
  , _action  :: Action
  } deriving (Show, Eq)

makeLenses ''Record

instance Ord Record where
  r1 `compare` r2
    | r1 ^. date == r2 ^. date = (r1 ^. time) `compare` (r2 ^. time)
    | otherwise =  (r1 ^. date) `compare` (r2 ^. date)

day4 :: IO Text
day4 = do
  input1 <- T.lines <$> readFile "input_day4_1.txt"
  let 
    sortedRecords = (sort . map parseRecord) input1 
    (_, (_, filledRecords)) = runState (fillGuardIds sortedRecords) (0, []) 
  mapM_ (putStrLn . Utils.showT) filledRecords
  pure ""

fillGuardIds :: [Record] -> State (Int, [Record]) ()
fillGuardIds (x:xs) = do
  (currId, result) <- get
  if x ^. guardId == 0
    then do
      put (currId, (x & guardId .~ currId) : result)
      fillGuardIds xs
    else do
      put (x ^. guardId, x : result)
      fillGuardIds xs

fillGuardIds [] = pure ()

-- [1518-11-01 00:00] Guard #10 begins shift
-- [1518-11-01 00:05] falls asleep
-- [1518-11-01 00:25] wakes up
parseRecord :: Text -> Record
parseRecord t =
  let
    str = T.unpack t
    _date = parseDate $ (take 10 . drop 1) str
    _time = parseTime $ (take 5 . drop 12) str
    _action = parseAction $ drop 19 str
    _guardId = parseGuardId $ drop 19 str
    in Record{..}
  where
    parseDate s =
      let
        _year = read $ take 4 s
        _month = read $ (take 2 . drop 5) s
        _day = read $ (take 2 . drop 8) s
        in Date{..}

    parseTime s =
      let
        _min = read $ take 2 s
        _sec = read $ (take 2 . drop 3) s
        in Time{..}

    parseAction s
     | "Guard" `isPrefixOf` s = Begins
     | "falls" `isPrefixOf` s = FallsAsleep
     | "wakes" `isPrefixOf` s = WakesUp
     | otherwise = panic $ "Can't parse action in string' " <> T.pack s <> " '"

    parseGuardId s
     | "Guard" `isPrefixOf` s = read $ (takeWhile isDigit . drop 1 . dropWhile (/= '#') ) s
     | otherwise = 0
