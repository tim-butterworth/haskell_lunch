module WaterPouring (solve) where

import Data.Map (Map, fromList, toList, lookup, keys, insert)
import Data.Map.Lazy (adjust)
import Data.List (find)
import Data.Set (Set, member, insert, fromList)

data Move = Fill Integer | Empty Integer | Pour Integer Integer
  deriving(Show)

constructMoveList :: [Integer] -> [Move]
constructMoveList buckets =
  [(Fill x) | x <- buckets] ++
  [(Empty x) | x <- buckets] ++
  [(Pour x y) | x <- buckets, y <- buckets, x/=y]

maybeUnwrap :: Maybe a -> a
maybeUnwrap (Just a) = a

delta :: Map Integer Integer -> Move -> Map Integer Integer
delta buckets (Fill n) = adjust (\_ -> n) n buckets
delta buckets (Empty n) = adjust (\_ -> 0) n buckets
delta buckets (Pour f t) =
  let f' = maybeUnwrap (Data.Map.lookup f buckets)
      t' = maybeUnwrap (Data.Map.lookup t buckets)
      adjustment = min (t - t') f'
  in adjust (\v -> v-adjustment) f (adjust (\v -> v+adjustment) t buckets)

updateState :: ((Map Integer Integer), [Move]) -> Move -> ((Map Integer Integer), [Move])
updateState (state, moves) m = ((delta state m), moves ++ [m])

alreadyVisited :: (Set (Map Integer Integer)) -> ((Map Integer Integer), [Move]) -> Bool
alreadyVisited set (state, _) = (member state set)

applyMoves :: [Move] -> Set (Map Integer Integer) -> ((Map Integer Integer), [Move]) -> [((Map Integer Integer), [Move])]
applyMoves moves visited state = filter (not.(alreadyVisited visited)) (map (updateState state) moves)

nextGeneration :: [Move] -> Set (Map Integer Integer) -> [((Map Integer Integer), [Move])] -> [((Map Integer Integer), [Move])]
nextGeneration moves visited generation = foldr (++) [] (map (applyMoves moves visited) generation)

updateVisited :: (Set (Map Integer Integer)) -> [((Map Integer Integer), [Move])] -> (Set (Map Integer Integer))
updateVisited visited ((s, _):ss) = updateVisited (Data.Set.insert s visited) ss
updateVisited visited [] = visited

generations :: [Move] -> Set (Map Integer Integer) -> [((Map Integer Integer), [Move])] -> [((Map Integer Integer), [Move])]
generations moves visited initial =
  let generation = nextGeneration moves visited initial
  in generation ++ if (0 == (length generation))
                   then []
                   else (generations moves (updateVisited visited generation) generation)

solutionState :: Integer -> ((Map Integer Integer), [Move]) -> Bool
solutionState t (mp, moves) = containsTheValue t (toList mp)
  where containsTheValue t [] = False
        containsTheValue t ((k,v):remainder) = if (v==t) then True else containsTheValue t remainder

solve :: [Integer] -> Integer -> Maybe ((Map Integer Integer), [Move])
solve buckets target =
  let initialMap = Data.Map.fromList (map (\x -> (x,0)) buckets)
  in find (solutionState target) (generations (constructMoveList buckets) (Data.Set.fromList [initialMap]) [(initialMap, [])])
