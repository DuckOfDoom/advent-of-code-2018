{-# LANGUAGE DeriveGeneric #-}

module Day2
( day2 )
where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T 
import           Protolude
import qualified Utils
import           Control.Lens

data Count = None | Two | Three | Both
  deriving (Show, Eq, Generic)

instance Hashable Count

data MatchState = MatchState 
  { _matchingChars :: [Char] 
  , _diffCount :: Int
  }
  deriving Show

defaultState :: MatchState
defaultState = MatchState [] 0
makeLenses ''MatchState

day2 :: IO Text
day2 = do
  input1 <- T.lines <$> readFile "input_day2_1.txt"
  let 
    answer1 = (calcChecksum . calcWordsCount) input1 

    pairs = Utils.mkPairsSelf (map T.unpack input1) -- convert input to a list of pairs to check for equality
    matchingStates = map (\p -> runState (findMatch p) defaultState) pairs
    answer2 = fmap (^. matchingChars) (find (\st -> st ^. diffCount == 1) (map snd matchingStates))
  pure $ mconcat
    [ Utils.showT answer1
    , ", "
    , T.pack $ fromMaybe "NO ANSWER" answer2
    ] 

calcChecksum :: HashMap Count Int -> Int
calcChecksum hm =
  let
    getCount = HM.lookupDefault 0 
    twosCount = getCount Two hm + getCount Both hm
    threesCount = getCount Three hm + getCount Both hm
  in
    twosCount * threesCount

calcWordsCount :: [Text] -> HashMap Count Int
calcWordsCount = foldl (\hm t -> (Utils.modifyOrAdd (calcCount t) (+1) 1 hm)) HM.empty
  where
    calcCount :: Text -> Count
    calcCount x
      | hasTwos && hasThrees = Both
      | hasTwos = Two
      | hasThrees = Three
      | otherwise = None
      where
        hasTwos = any (\(_, c) -> c == 2) (countsMap x)
        hasThrees = any (\(_, c) -> c == 3) (countsMap x)
        countsMap s = HM.toList (fillMap (T.unpack s))
        fillMap :: [Char] -> HashMap Char Integer
        fillMap = foldl (\hm ch -> (Utils.modifyOrAdd ch (+1) 1 hm)) HM.empty 


findMatch :: ([Char], [Char]) -> State MatchState ()
findMatch (a@(x:xs), b@(y:ys))
  | length a /= length b = pure ()
  | otherwise = do
    st <- get
    if st ^. diffCount > 1
      then
        pure ()
      else do
        if x /= y
          then
            put (st & diffCount %~ (+1))
          else
            put (st & matchingChars %~ (x :))
        findMatch (xs, ys) 
    pure ()
    
-- reverse chars since we append them from back to front
findMatch _ = modify (\st -> st & matchingChars %~ reverse)