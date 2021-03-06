module Day1
( day1 )
where

import           Control.Lens
import qualified Data.Text    as T
import           Prelude      (read)
import           Protolude
import qualified Utils
import qualified Data.HashMap.Strict as HM

data PuzzleState = PuzzleState
  { _current :: Int
  , _passed  :: HM.HashMap Int Int
  }

makeLenses ''PuzzleState

day1 :: IO Text
day1 = do
  input1 <- T.lines <$> readFile "input_day1_1.txt"
  input2 <- T.lines <$> readFile "input_day1_2.txt"
  let answer1 = Utils.showT $ calculateAnswer1 input1
      answer2 = Utils.showT $ calculateAnswer2 input2
  pure (mconcat [answer1, ", " , answer2])

calculateAnswer1 :: [Text] -> Int
calculateAnswer1 = foldl (flip parseFunc) 0

calculateAnswer2 :: [Text] -> Int
calculateAnswer2 input = fst $ runState (calcWithState (cycle input)) (PuzzleState 0 HM.empty)
  where
    calcWithState :: [Text] -> State PuzzleState Int
    calcWithState (x:xs) = do
      st <- get
      case checkPassedTwice (st ^. passed) of
        Just a -> pure a
        Nothing -> do
          put (updateState x st)
          calcWithState xs

      where
        updateState :: Text -> PuzzleState -> PuzzleState
        updateState strFunc st =
          let
            newCurrent = parseFunc strFunc $ st ^. current
          in
            PuzzleState
            { _current = newCurrent
            , _passed = Utils.modifyOrAdd newCurrent (+1) 1 (st ^. passed)
            }

        checkPassedTwice :: HM.HashMap Int Int -> Maybe Int
        checkPassedTwice hm = do
         res <- find (\(_, v) -> v >= 2) (HM.toList hm)
         case res of
          (k, _) -> Just k

    calcWithState [] = pure 0

parseFunc :: Text -> (Int -> Int)
parseFunc = convert . T.unpack
    where
      convert (s:xs) = case s of
        '+' -> (+ (read xs :: Int))
        '-' -> (subtract (read xs :: Int))
        _   -> identity
      convert [] = identity
