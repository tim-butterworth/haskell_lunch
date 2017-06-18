module Garden
    ( Plant (..)
    , defaultGarden
    , garden
    , lookupPlants
    ) where

import Data.Map (Map, empty, fromList, lookup)
import Data.Maybe (fromMaybe, catMaybes)
import Data.List (sort)

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

plantToPlant :: Char -> Plant
plantToPlant 'C' = Clover
plantToPlant 'G' = Grass
plantToPlant 'R' = Radishes
plantToPlant 'V' = Violets

students = ["Alice", "Bob", "Charlie", "David", "Eve", "Fred", "Ginny", "Harriet", "Ileana", "Joseph", "Kincaid", "Larry"]

mapLookup :: Ord k => k -> Map k b -> Maybe b
mapLookup k mp = Data.Map.lookup k mp

defaultGarden :: String -> Map String [Plant]
defaultGarden plants = garden students plants

chop :: [a] -> (Int, Int) -> [[a]] -> [[a]]
chop [] _ accume = accume
chop lst (v, max) [] = chop lst (v,max) [[]]
chop (x:xs) (v, max) accume | (v == max) = chop (x:xs) (0, max) (accume ++ [[]])
                            | otherwise = chop xs ((v+1), max) ((init accume) ++ [((last accume) ++ [x])])

partition :: Int -> [a] -> [[a]]
partition n lst = chop lst (0, n) []

mergeRowPartitions :: Foldable t => t [[a]] -> [[a]]
mergeRowPartitions lst = foldr smoosh (repeat []) lst
  where joinListTuple (a,b) = b++a
        smoosh accume lst = (map joinListTuple) (zip lst accume)

chopUpGardenString :: [Char] -> [[Char]]
chopUpGardenString plants = (mergeRowPartitions (map (partition 2) (lines plants)))

garden :: [String] -> String -> Map String [Plant]
garden students plants = fromList (zip (sort students) ((toPlants.chopUpGardenString) plants))
  where toPlants lst = map (map plantToPlant) lst

lookupPlants :: String -> Map String [Plant] -> [Plant]
lookupPlants student = comp (fromMaybe []) (mapLookup student)

comp :: (c -> b) -> (a -> c) -> a -> b
comp f g v = f (g v)
