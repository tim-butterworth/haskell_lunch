module SudokuSolver (Board, Entry(..), Row, Column, solve) where

import Data.Map (Map, fromList, toList, empty, lookup, keys, insert)
import Data.List (find)

data Entry = V Int
           | Empty
             deriving (Show, Eq)

data Board = Board [[Entry]]
  deriving (Show)

data Row = Row Int
  deriving (Show, Eq, Ord)

data Column = Column Int
  deriving (Show, Eq, Ord)

data Coordinate = Coordinate Row Column
  deriving (Show, Eq, Ord)

toCoordinate :: (Int, Int) -> Coordinate
toCoordinate (row, column) = Coordinate (Row row) (Column column)

getColumn :: Int -> [Coordinate]
getColumn c = map toCoordinate (zip [0..8] (repeat c))

getRow :: Int -> [Coordinate]
getRow r = map toCoordinate (zip (repeat r) [0..8])

getSection :: Int -> Int -> [Coordinate]
getSection row column =
  let makeArray n = [(3*n)..((3*n) + 2)]
      rowValues = makeArray (div row 3)
      columnValues = makeArray (div column 3)
  in map (\(r, c) -> Coordinate (Row r) (Column c)) [(r,c) | r <- rowValues, c <- columnValues]

squash :: [[Entry]] -> [(Coordinate, (Entry, [Int]))] -> Int -> [(Coordinate, (Entry, [Int]))]
squash [] accume _ = accume
squash (c:cs) accume column = squash cs (accume ++ (sectionize c column)) (column + 1)
  where sectionize entries column = zip (map toCoordinate (zip [0..8] (repeat column))) (zip entries (repeat [1..9]))

convertToSolvable :: Board -> Map Coordinate (Entry, [Int])
convertToSolvable (Board grid) = fromList (squash grid [] 0)
        
partitions :: Map Coordinate [Coordinate]
partitions =
  let coordinates = [(x,y) | x <- [0..8], y <- [0..8]]
      collectEntries column row = (getColumn column) ++ (getRow row) ++ (getSection row column)
      makePartition (row, column) = ((toCoordinate (row, column)), (collectEntries column row))
  in fromList (map makePartition coordinates)

removeOption :: Int -> Maybe (Entry, [Int]) -> (Entry, [Int])
removeOption _ (Just (V v, lst)) = (V v, lst)
removeOption r (Just (Empty, lst)) = (Empty, (filter (r/=) lst))

applyRestriction :: Map Coordinate (Entry, [Int]) -> Coordinate -> Int -> [Coordinate] -> Map Coordinate (Entry, [Int])
applyRestriction mp coordinate v [] = insert coordinate (V v, [v]) mp
applyRestriction mp coordinate v (p:ps) = applyRestriction (insert p (removeOption v (Data.Map.lookup p mp)) mp) coordinate v ps

maybeUnwrap :: Maybe a -> a
maybeUnwrap (Just a) = a
  
initializeCell :: Coordinate -> Maybe (Entry, [Int]) -> Map Coordinate (Entry, [Int]) -> Map Coordinate (Entry, [Int])
initializeCell coordinate (Just (Empty, _)) mp = mp
initializeCell coordinate (Just (V n, entries)) mp = applyRestriction mp coordinate n (maybeUnwrap (Data.Map.lookup coordinate (partitions)))

initializeByCells :: [Coordinate] -> Map Coordinate (Entry, [Int]) -> Map Coordinate (Entry, [Int])
initializeByCells [] mp = mp
initializeByCells (c:cs) mp = initializeByCells cs (initializeCell c (Data.Map.lookup c mp) mp)

initialize :: Map Coordinate (Entry, [Int]) -> Map Coordinate (Entry, [Int])
initialize mp =
  let cells = keys mp
  in initializeByCells cells mp

nextOption :: Foldable t => Map t1 (Entry, t a) -> Maybe (t1, (Entry, t a))
nextOption optionMap = find isFullyQualified (Data.Map.toList optionMap)
  where isFullyQualified (c, (v, lst)) = ((length lst) == 1) && (v == Empty)

doMoves :: Map Coordinate (Entry, [Int]) -> Map Coordinate (Entry, [Int])
doMoves optionMap = applyAllTheMoves optionMap (nextOption optionMap)
  where applyAllTheMoves mp Nothing = mp
        applyAllTheMoves mp (Just (c, (e, [v]))) =
          let nextMap = (applyRestriction mp c v (maybeUnwrap (Data.Map.lookup c (partitions))))
          in applyAllTheMoves nextMap (nextOption nextMap)

toBoard :: Map Coordinate (Entry, [Int]) -> Board
toBoard mp = Board (foldr (\c b -> (getEntries c mp):b) [] [0..8])
  where getEntries c mp = foldr (\r b -> (getEntry (maybeUnwrap (Data.Map.lookup (toCoordinate (r, c)) mp))):b) [] [0..8]
        getEntry (e, [v]) = e

solve :: Board -> Board
solve b = toBoard (doMoves (initialize (convertToSolvable b)))

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

--[[V 1,V 3,V 9,V 2,V 5,V 7,V 4,V 6,V 8],
-- [V 8,V 4,V 7,V 1,V 6,V 3,V 5,V 9,V 2],
-- [V 2,V 5,V 6,V 4,V 9,V 8,V 7,V 3,V 1],
-- [V 6,V 9,V 4,V 3,V 8,V 5,V 2,V 1,V 7],
-- [V 3,V 8,V 2,V 7,V 1,V 4,V 9,V 5,V 6],
-- [V 7,V 1,V 5,V 6,V 2,V 9,V 8,V 4,V 3],
-- [V 9,V 2,V 3,V 8,V 4,V 6,V 1,V 7,V 5],
-- [V 4,V 7,V 1,V 5,V 3,V 2,V 6,V 8,V 9],
-- [V 5,V 6,V 8,V 9,V 7,V 1,V 3,V 2,V 4]]
