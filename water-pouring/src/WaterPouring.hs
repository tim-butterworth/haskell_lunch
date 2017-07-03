module WaterPouring (solve, Move(..), nextMoves, allPours) where

import Data.Map (Map, fromList, toList, lookup, keys, insert, mapWithKey)
import Data.Map.Lazy (adjust, elems)
import Data.Maybe (fromJust)

data Move = Fill Int | Empty Int | Pour Int Int
  deriving(Show, Eq)

nextMoves' :: [Int] -> [Int] -> [Move]
nextMoves' _ [] = []
nextMoves' _ (b:[]) = [(Fill b), (Empty b)]
nextMoves' acc (b:bs) = (nextMoves' [] [b]) ++ (nextMoves' [b] bs) ++ (allPours acc b)
--nextMoves'  = (nextMoves' [] [b]) ++ (nextMoves' [b] bs) ++ (allPours acc b)


allPours :: [Int] -> Int -> [Move]
allPours [] _ = []
allPours (b:bs) b' = [(Pour b b'), (Pour b' b)] ++ (allPours bs b')

nextMoves :: [Int] -> [Move]
nextMoves = nextMoves' []

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



--possibilities :: ((Map Int Int), [Move]) -> [((Map Int Int), [Move])]
--possibilities (state, movesSoFar) = map (\move -> ((delta state move), movesSoFar++[move])) possibleMoves


--Data.Map.fromList [(5, 2), (3, 0)]
