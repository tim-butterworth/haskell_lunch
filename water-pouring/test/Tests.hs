import Test.Hspec
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import WaterPouring (solve, Move(..))

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
