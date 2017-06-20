module WaterPouring (solve) where

import Data.Map (Map, fromList, toList, lookup, keys, insert)
import Data.Map.Lazy (adjust)

data Move = Fill Int | Empty Int | Pour Int Int
  deriving(Show)

-- 5 3
solve :: [Int] -> Int -> [Move]
solve buckets target = [] -- solveWithState buckets [0, 0] target

--solveWithState :: [Int] -> [Int] -> Int -> [Move]
--solveWithState buckets currentValues target =

delta :: Map Int Int -> Move -> Map Int Int
delta buckets (Fill n) = adjust (\_ -> n) n buckets
delta buckets (Empty n) = adjust (\_ -> 0) n buckets
delta buckets (Pour s d) =

  let s' =


--Data.Map.fromList [(5, 2), (3, 0)]