module Helper.Map
  ( insertMany
  ) where

import Data.Ord
import Data.Map (Map, insert)
import Prelude

insertMany :: Ord k => [(k, a)] -> Map k a -> Map k a
insertMany kvs m = foldr (\(k, v) acc -> insert k v acc) m kvs
