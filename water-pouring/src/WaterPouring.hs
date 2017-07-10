module WaterPouring (solve, Move(..), nextMoves) where

import Data.Map (Map, fromList, toList, lookup, keys, insert, mapWithKey)
import Data.List (delete)
import Data.Map.Lazy (adjust, elems)
import Data.Maybe (fromJust)

data Move = Fill Int | Empty Int | Pour Int Int
  deriving(Show, Eq)

emptyMoves :: [Int] -> [Move]
emptyMoves = map (Empty)

fillMoves :: [Int] -> [Move]
fillMoves = map (Fill)

pourMoves :: [Int] -> [Move]
pourMoves list = foldl (\acc x -> acc ++ (map (Pour x)) (delete x list) ) [] list

nextMoves :: [Int] -> [Move]
nextMoves lst = (emptyMoves lst) ++ (fillMoves lst) ++ (pourMoves lst)

solve :: [Int] -> Int -> Maybe [Move]
solve buckets target = find solved (nextMoves buckets)

initialState:: [Int] -> Map Int Int
initialState buckets = fromList (zip buckets (repeat 0))

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




--possibilities :: ((Map Int Int), [Move]) -> [((Map Int Int), [Move])]
--possibilities (state, movesSoFar) = map (\move -> ((delta state move), movesSoFar++[move])) possibleMoves


--Data.Map.fromList [(5, 2), (3, 0)]
