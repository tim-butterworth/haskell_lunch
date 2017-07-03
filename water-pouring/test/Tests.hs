import Test.Hspec
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import WaterPouring (solve, Move(..), nextMoves, allPours)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "solve should solve the water problem" $ do
    xcontext "unsolvable puzzles" $ do

      it "will return 'Nothing' if the list of buckets is empty" $ do
        (solve [] 9) `shouldBe` Nothing

--      it "will return an empty list of moves if there is not solution" $ do
--        (solve [4, 12] 2) `shouldBe` Nothing

    xcontext "solvable puzzles" $ do

      it "solves the simplest puzzle" $ do
        (solve [9] 9) `shouldBe` Just [(Fill 9)]

    context "#nextMoves" $ do

      it "generates an empty list for an empty list" $ do
        (nextMoves []) `shouldBe` []

      it "generates options for a single bucket" $ do
        (nextMoves [9]) `shouldBe` [(Fill 9), (Empty 9)]

      xit "generates options for two bucket" $ do
        (nextMoves [1, 2]) `shouldBe` [(Fill 1), (Empty 1), (Fill 2), (Empty 2), (Pour 1 2), (Pour 2 1)]

    context "#allPours" $ do
      it "xasx" $ do
        (allPours [] 1) `shouldBe` []
      it "xxxx" $ do
        (allPours [1] 2) `shouldBe` [(Pour 1 2), (Pour 2 1)]
      it "yyy" $ do
        (allPours [1, 2] 3) `shouldMatchList` [(Pour 1 2), (Pour 2 1), (Pour 2 3), (Pour 3 2), (Pour 1 3), (Pour 3 1)]
