import Test.Hspec
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import WaterPouring (solve, Move(..), nextMoves, delta)
import Data.Map (Map, fromList, toList, lookup, keys, insert, mapWithKey)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "solve should solve the water problem" $ do
    context "unsolvable puzzles" $ do

      it "will return 'Nothing' if the list of buckets is empty" $ do
        (solve [] 9) `shouldBe` Nothing

      it "will return an empty list of moves if there is not solution" $ do
        (solve [4, 12] 2) `shouldBe` Nothing

    context "solvable puzzles" $ do

      it "solves the simplest puzzle" $ do
        (solve [9] 9) `shouldBe` Just [(Fill 9)]

      it "solves a simple puzzle" $ do
        (solve [5, 3] 2) `shouldBe` Just [(Fill 5), (Pour 5 3)]

      it "solves the movie puzzle" $ do
        (solve [5, 3] 4) `shouldBe` Just [Fill 5,Pour 5 3,Empty 3,Pour 5 3,Fill 5,Pour 5 3]

    context "#nextMoves" $ do

      it "generates an empty list for an empty list" $ do
        (nextMoves []) `shouldBe` []

      it "generates options for a single bucket" $ do
        (nextMoves [9]) `shouldMatchList` [(Fill 9), (Empty 9)]

      it "generates options for two bucket" $ do
        (nextMoves [1, 2]) `shouldMatchList` [(Fill 1), (Empty 1), (Fill 2), (Empty 2), (Pour 1 2), (Pour 2 1)]

    context "#delta" $ do
      it "pours when dest is empty" $ do
        (delta (fromList [(3,3), (5,0)]) (Pour 3 5)) `shouldBe` (fromList [(3,0), (5,3)])
      it "pours when dest is NOT empty" $ do
        (delta (fromList [(3,3), (5,3)]) (Pour 3 5)) `shouldBe` (fromList [(3,1), (5,5)])
      it "empty when dest is NOT full" $ do
        (delta (fromList [(3,3), (5,3)]) (Empty 5)) `shouldBe` (fromList [(3,3), (5,0)])
      it "fill when dest is NOT empty" $ do
        (delta (fromList [(3,2), (5,3)]) (Fill 5)) `shouldBe` (fromList [(3,2), (5,5)])
