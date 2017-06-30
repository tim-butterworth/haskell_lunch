module WaterPouring (solve, Move(..)) where

import Data.Map (Map, fromList, toList, lookup, keys, insert, mapWithKey)
import Data.Map.Lazy (adjust, elems)
import Data.Maybe (fromJust)

data Move = Fill Int | Empty Int | Pour Int Int
  deriving(Show, Eq)

-- 5 3
solve :: [Int] -> Int -> Maybe [Move]
solve buckets target = Nothing

--solveWithState :: [Int] -> [Int] -> Int -> [Move]
--solveWithState buckets currentValues target =

delta :: Map Int Int -> Move -> Map Int Int
delta buckets (Fill n) = adjust (\_ -> n) n buckets
delta buckets (Empty n) = adjust (\_ -> 0) n buckets
delta buckets (Pour s d) =
  let s' = fromJust (Data.Map.lookup s buckets)
      d' = fromJust (Data.Map.lookup d buckets)
      difference = min (d - d') s'
      adjuster capacity v | capacity == s = v - difference
                          | capacity == d = v + difference
                          | otherwise = v
  in mapWithKey adjuster buckets

solved :: Int -> Map Int Int -> Bool
solved target state = elem target (elems state)

possibleMoves = [(Fill 5), (Fill 3), (Empty 5), (Empty 3), (Pour 5 3), (Pour 3 5)]

possibilities :: ((Map Int Int), [Move]) -> [((Map Int Int), [Move])]
possibilities (state, movesSoFar) = map (\move -> ((delta state move), movesSoFar++[move])) possibleMoves


--Data.Map.fromList [(5, 2), (3, 0)]
