module SudokuSolver (Board, Entry(..), Row, Column, solve) where

import Data.Map (Map, fromList, toList, empty, lookup, keys, insert)
import Data.List (find, sortBy)

data Entry = V Int | Empty
  deriving (Show, Eq)

data Board = Board [[Entry]]
  deriving (Show, Eq)

data Row = Row Int
  deriving (Show, Eq, Ord)

data Column = Column Int
  deriving (Show, Eq, Ord)

data Coordinate = Coordinate Row Column
  deriving (Show, Eq, Ord)

data Move = Move (Coordinate, Int)
  deriving (Show)

toCoordinate :: (Int, Int) -> Coordinate
toCoordinate (row, column) = Coordinate (Row row) (Column column)

toMove :: Coordinate -> Int -> Move
toMove c v = Move (c, v)

getColumn :: Int -> [Coordinate]
getColumn c = map toCoordinate (zip [0..8] (repeat c))

getRow :: Int -> [Coordinate]
getRow r = map toCoordinate (zip (repeat r) [0..8])

getSection :: Int -> Int -> [Coordinate]
getSection row column =
  let makeArray n = [(3*n)..((3*n) + 2)]
      rowValues = makeArray (div row 3)
      columnValues = makeArray (div column 3)
  in map toCoordinate [(r,c) | r <- rowValues, c <- columnValues]

initial :: Map Coordinate (Entry, [Int])
initial = fromList (zip (map toCoordinate [(x,y) | x <- [0..8], y <- [0..8]]) (repeat (Empty, [1..9])))

partitions :: Map Coordinate [Coordinate]
partitions =
  let coordinates = [(x,y) | x <- [0..8], y <- [0..8]]
      collectEntries column row = (getColumn column) ++ (getRow row) ++ (getSection row column)
      makePartition (row, column) = ((toCoordinate (row, column)), (collectEntries column row))
  in fromList (map makePartition coordinates)

removeOption :: Int -> Maybe (Entry, [Int]) -> (Entry, [Int])
removeOption _ (Just (V v, lst)) = (V v, lst)
removeOption r (Just (Empty, lst)) = (Empty, (filter (r/=) lst))

maybeUnwrap :: Maybe a -> a
maybeUnwrap (Just a) = a

toBoard :: Map Coordinate (Entry, [Int]) -> Board
toBoard mp = Board (foldr (\c b -> (getEntries c mp):b) [] [0..8])
  where getEntries c mp = foldr (\r b -> (getEntry (maybeUnwrap (Data.Map.lookup (toCoordinate (r, c)) mp))):b) [] [0..8]
        getEntry (e, v) = e

getChoices :: Map Coordinate (Entry, [Int]) -> [(Coordinate, (Entry, [Int]))]
getChoices mp = sortBy shortListFirst (filter isEmpty (Data.Map.toList mp))
  where isEmpty (c, (Empty, vals)) = True
        isEmpty _ = False
        shortListFirst (_, (_, lstc)) (_, (_, lstd)) = compare (length lstc) (length lstd)

convertBoardToMoves :: Board -> [Move]
convertBoardToMoves (Board b) = traverseBoard 0 b []
  where traverseBoard rIndex (c:cs) accume = traverseBoard (rIndex+1) cs (accume ++ (traverseColumn rIndex 0 c []))
        traverseBoard _ [] accume = accume
        traverseColumn rIndex cIndex (e:es) accume | (e == Empty) = traverseColumn rIndex (cIndex+1) es accume
                                                   | otherwise = traverseColumn rIndex (cIndex+1) es ((createMove rIndex cIndex e) : accume)
        traverseColumn _ _ [] accume = accume
        createMove rIndex cIndex e = Move ((toCoordinate (cIndex, rIndex)), (valueOf e))
        valueOf (V v) = v

applyRestrictions :: Map Coordinate (Entry, [Int]) -> Int -> [Coordinate] -> Map Coordinate (Entry, [Int])
applyRestrictions mp _ [] = mp
applyRestrictions mp v (p:ps) = applyRestrictions (insert p (removeOption v (Data.Map.lookup p mp)) mp) v ps

toMoves :: (Coordinate, (Entry, [Int])) -> [Move]
toMoves (c, (_, lst)) = map (toMove c) lst

fullySpecified :: (Coordinate, (Entry, [Int])) -> Bool
fullySpecified (c, (Empty, [a])) = True
fullySpecified _ = False

getFullySpecifiedMove :: Map Coordinate (Entry, [Int]) -> Maybe Move
getFullySpecifiedMove mp = fmap (head.toMoves) (find fullySpecified (toList mp))

cascade :: Map Coordinate (Entry, [Int]) -> Map Coordinate (Entry, [Int])
cascade mp = doCascade mp (getFullySpecifiedMove mp)
  where doCascade mp (Just move) = cascade (applyMove mp move)
        doCascade mp Nothing = mp

applyMove :: (Map Coordinate (Entry, [Int])) -> Move -> (Map Coordinate (Entry, [Int]))
applyMove state (Move (c, v)) = (updateMap state c v)
  where updateMap state c v = ((applyRestrictions (insert c (V v, [v]) state) v) (maybeUnwrap (Data.Map.lookup c partitions)))

applyMoves :: Map Coordinate (Entry, [Int]) -> [Move] -> Map Coordinate (Entry, [Int])
applyMoves mp = foldr (flip applyMove) mp

applyBoardDefinition :: Board -> (Map Coordinate (Entry, [Int]))
applyBoardDefinition = (applyMoves (initial)).convertBoardToMoves

nextMoves :: Map Coordinate (Entry, [Int]) -> [Move]
nextMoves = toMoves.head.getChoices

nextGeneration :: [Map Coordinate (Entry, [Int])] -> [Map Coordinate (Entry, [Int])]
nextGeneration [] = []
nextGeneration (m:ms) = (map (cascade.(applyMove m)) (nextMoves m)) ++ (nextGeneration ms)

solved :: Map Coordinate (Entry, [Int]) -> Bool
solved m = 0 == (length (getChoices m))

applyGuesses :: Map Coordinate (Entry, [Int]) -> Map Coordinate (Entry, [Int])
applyGuesses m = breadthFirstSearch [m]
  where breadthFirstSearch lst =
          let maybeSolution = find solved lst
          in if (not (Nothing == maybeSolution))
             then (maybeUnwrap maybeSolution)
             else breadthFirstSearch (nextGeneration lst)

arrayWrap :: a -> [a]
arrayWrap v = [v]

solve :: Board -> Board
solve = toBoard.applyGuesses.applyBoardDefinition

-- Sample boards --
toEntry :: Int -> Entry
toEntry 0 = Empty
toEntry v = V v

sampleBoard = Board 
 (map (\column -> map toEntry column)
  [[0, 3, 9,   0, 0, 7,   4, 0, 8]
  ,[8, 0, 7,   0, 6, 3,   0, 0, 2]
  ,[0, 5, 0,   0, 0, 8,   7, 0, 0]
  ,[0, 9, 4,   3, 0, 0,   0, 1, 0]
  ,[0, 0, 2,   7, 0, 0,   9, 5, 0]
  ,[0, 1, 0,   6, 2, 0,   0, 0, 3]
  ,[9, 0, 0,   0, 4, 6,   1, 0, 0]
  ,[4, 0, 1,   0, 0, 0,   6, 8, 0]
  ,[5, 6, 0,   9, 0, 1,   0, 2, 0]])

-- [
--    [1,3,9,2,5,7,4,6,8]
--   ,[8,4,7,1,6,3,5,9,2]
--   ,[2,5,6,4,9,8,7,3,1]
--   ,[6,9,4,3,8,5,2,1,7]
--   ,[3,8,2,7,1,4,9,5,6]
--   ,[7,1,5,6,2,9,8,4,3]
--   ,[9,2,3,8,4,6,1,7,5]
--   ,[4,7,1,5,3,2,6,8,9]
--   ,[5,6,8,9,7,1,3,2,4]
-- ]

sampleBoard2 = Board 
 (map (\column -> map toEntry column)
  [[3, 1, 0,   2, 0, 0,   0, 0, 5]
  ,[9, 7, 0,   4, 0, 0,   0, 6, 1]
  ,[0, 4, 0,   1, 7, 6,   0, 0, 9]
  ,[1, 0, 8,   0, 0, 2,   9, 3, 0]
  ,[4, 0, 3,   0, 9, 0,   0, 0, 6]
  ,[6, 0, 0,   0, 0, 3,   4, 8, 0]
  ,[0, 0, 9,   0, 2, 0,   0, 0, 0]
  ,[0, 6, 0,   0, 0, 8,   1, 7, 3]
  ,[5, 0, 0,   7, 6, 0,   0, 0, 8]])

-- [
--    [3,1,6,2,8,9,7,4,5]
--   ,[9,7,2,4,3,5,8,6,1]
--   ,[8,4,5,1,7,6,3,2,9]
--   ,[1,5,8,6,4,2,9,3,7]
--   ,[4,2,3,8,9,7,5,1,6]
--   ,[6,9,7,5,1,3,4,8,2]
--   ,[7,8,9,3,2,1,6,5,4]
--   ,[2,6,4,9,5,8,1,7,3]
--   ,[5,3,1,7,6,4,2,9,8]
-- ]

sampleBoardTricky = Board
  (map (\column -> map toEntry column)
   [[0, 0, 0,   8, 0, 1,   0, 0, 0]
   ,[0, 0, 0,   0, 0, 0,   0, 4, 3]
   ,[5, 0, 0,   0, 0, 0,   0, 0, 0]
   ,[0, 0, 0,   0, 7, 0,   8, 0, 0]
   ,[0, 0, 0,   0, 0, 0,   1, 0, 0]
   ,[0, 2, 0,   0, 3, 0,   0, 0, 0]
   ,[6, 0, 0,   0, 0, 0,   0, 7, 5]
   ,[0, 0, 3,   4, 0, 0,   0, 0, 0]
   ,[0, 0, 0,   2, 0, 0,   6, 0, 0]])

-- [
--   [2,3,7, 8,4,1, 5,6,9]
--  ,[1,8,6, 7,9,5, 2,4,3]
--  ,[5,9,4, 3,2,6, 7,1,8]

--  ,[3,1,5, 6,7,4, 8,9,2]
--  ,[4,6,9, 5,8,2, 1,3,7]
--  ,[7,2,8, 1,3,9, 4,5,6]

--  ,[6,4,2, 9,1,8, 3,7,5]
--  ,[8,5,3, 4,6,7, 9,2,1]
--  ,[9,7,1, 2,5,3, 6,8,4]
-- ]

sampleBoardLessTricky = Board
  (map (\column -> map toEntry column)
   [[8, 0, 0,  0, 0, 0,  0, 0, 0]
   ,[0, 0, 3,  6, 0, 0,  0, 0, 0]
   ,[0, 7, 0,  0, 9, 0,  2, 0, 0]
   ,[0, 5, 0,  0, 0, 7,  0, 0, 0]
   ,[0, 0, 0,  0, 4, 5,  7, 0, 0]
   ,[0, 0, 0,  1, 0, 0,  0, 3, 0]
   ,[0, 0, 1,  0, 0, 0,  0, 6, 8]
   ,[0, 0, 8,  5, 0, 0,  0, 1, 0]
   ,[0, 9, 0,  0, 0, 0,  4, 0, 0]])

-- [
--    [8,1,2, 7,5,3, 6,4,9]
--   ,[9,4,3, 6,8,2, 1,7,5]
--   ,[6,7,5, 4,9,1, 2,8,3]

--   ,[1,5,4, 2,3,7, 8,9,6]
--   ,[3,6,9, 8,4,5, 7,2,1]
--   ,[2,8,7, 1,6,9, 5,3,4]

--   ,[5,2,1, 9,7,4, 3,6,8]
--   ,[4,3,8, 5,2,6, 9,1,7]
--   ,[7,9,6, 3,1,8, 4,5,2]
-- ]
