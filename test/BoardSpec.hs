{- |
BoardSpec.hs
Spec for the Board module.
-}
module BoardSpec (
main, 
spec
) where

import Test.Hspec
import Board

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "board" $ do
    it "shows a square" $ do
      showSquare 0 `shouldBe` "a1"
      showSquare 28 `shouldBe` "e4"
      showSquare 63 `shouldBe` "h8" 