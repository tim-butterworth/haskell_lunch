module Garden
    ( Plant (..)
    , defaultGarden
    , garden
    , lookupPlants
    ) where

import Data.Map (Map, empty, fromList, lookup)
import Data.Maybe (fromMaybe, catMaybes)

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

defaultGarden :: String -> Map String [Plant]
defaultGarden plants = garden ["Alice"] plants

garden :: [String] -> String -> Map String [Plant]
garden students plants = fromList [("Alice", rows plants)]
  where rows plants


lookupPlants :: String -> Map String [Plant] -> [Plant]
lookupPlants student garden = fromMaybe [] (Data.Map.lookup student garden)
