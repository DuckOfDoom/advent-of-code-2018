module Utils where

import Protolude
-- import GHC.Show
import qualified Data.Text as T

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

showT :: (Show a) => a -> Text
showT = T.pack . show

modifyOrAdd ::(Eq k, Hashable k) => k -> (v -> v) -> v -> HashMap k v -> HashMap k v
modifyOrAdd key func value hmap 
  | HM.member key hmap = HM.adjust func key hmap
  | otherwise = HM.insert key value hmap
  