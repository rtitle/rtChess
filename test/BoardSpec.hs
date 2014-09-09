{- |
BoardSpec.hs
Spec for the Board module.
-}
module BoardSpec (
main, 
spec
) where

import Test.Hspec
import qualified Data.Vector as V
import Data.Maybe
import Control.Applicative
import Board

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Piece" $ do
    describe "show" $ do
      it "shows white pieces" $ do
        show (Piece King White 0) `shouldBe` "K"
        show (Piece Queen White 0) `shouldBe` "Q"
        show (Piece Rook White 0) `shouldBe` "R"
        show (Piece Bishop White 0) `shouldBe` "B"
        show (Piece Knight White 0) `shouldBe` "N"
        show (Piece Pawn White 0) `shouldBe` "P"
      it "shows black pieces" $ do  
        show (Piece King Black 0) `shouldBe` "k"
        show (Piece Queen Black 0) `shouldBe` "q"
        show (Piece Rook Black 0) `shouldBe` "r"
        show (Piece Bishop Black 0) `shouldBe` "b"
        show (Piece Knight Black 0) `shouldBe` "n"
        show (Piece Pawn Black 0) `shouldBe` "p"
        
  describe "readPieceType" $ do
    it "reads white piece types" $ do
      readPieceType 'K' `shouldBe` Just King
      readPieceType 'Q' `shouldBe` Just Queen
      readPieceType 'R' `shouldBe` Just Rook
      readPieceType 'B' `shouldBe` Just Bishop
      readPieceType 'N' `shouldBe` Just Knight
      readPieceType 'P' `shouldBe` Just Pawn
      readPieceType 'x' `shouldBe` Nothing
    it "reads black piece types" $ do
      readPieceType 'k' `shouldBe` Just King
      readPieceType 'q' `shouldBe` Just Queen
      readPieceType 'r' `shouldBe` Just Rook
      readPieceType 'b' `shouldBe` Just Bishop
      readPieceType 'n' `shouldBe` Just Knight
      readPieceType 'p' `shouldBe` Just Pawn
     
  describe "showSquare" $ do
    it "shows a square" $ do
      showSquare 0 `shouldBe` "a1"
      showSquare 28 `shouldBe` "e4"
      showSquare 63 `shouldBe` "h8" 
    
  describe "readSquare" $ do
    it "reads a square" $ do
      readSquare "b1" `shouldBe` 1
      readSquare "g8" `shouldBe` 62
      readSquare "xxx" `shouldBe` -1 
      
  describe "toBoard" $ do
    it "converts empty Pieces to an empty Board" $ do
      toBoard emptyPieces `shouldBe` V.replicate 64 Nothing
    it "converts a non-empty Pieces to a non-empty Board" $ do
      toBoard sicilianDragon `shouldSatisfy` liftA2 (&&) (\b -> V.length b == 64) (V.any isJust)
    it "correctly converts a Pieces to a Board" $ do
      let pcs = [Piece Rook White $ readSquare "a1", Piece Queen Black $ readSquare "b1", Piece Knight White $ readSquare "c1"] 
      toBoard pcs `shouldBe` (V.fromList $ (map Just pcs) ++ replicate (64-length pcs) Nothing)
      
  describe "prettyBoard" $ do
    it "returns a printable board" $ do
      let verifyOccurences n c b = (length . filter (==c) $ b) == n
      prettyBoard (toBoard sicilianDragon) `shouldSatisfy` liftA2 (&&) (not . null) (verifyOccurences (9*8+9*2) '|')
    it "is consistent with prettyPieces" $ do
      prettyBoard (toBoard sicilianDragon) `shouldBe` prettyPieces sicilianDragon
      
  describe "file" $ do
    it "returns the correct file" $ do
      file 0 `shouldBe` 0
      file 2 `shouldBe` 2
      file 14 `shouldBe` 6
      file 63 `shouldBe` 7
  
  describe "rank" $ do
    it "returns the correct rank" $ do
      rank 0 `shouldBe` 0
      rank 1 `shouldBe` 0
      rank 8 `shouldBe` 1
      rank 55 `shouldBe` 6
      
  describe "occupant" $ do
    it "returns the occupant of a non-empty square" $ do
      let b = toBoard initialPieces
      occupant b (readSquare "a1") `shouldBe` (Just $ Piece Rook White (readSquare "a1"))
      occupant b (readSquare "d1") `shouldBe` (Just $ Piece Queen White (readSquare "d1"))
      occupant b (readSquare "c2") `shouldBe` (Just $ Piece Pawn White (readSquare "c2"))
      occupant b (readSquare "a7") `shouldBe` (Just $ Piece Pawn Black (readSquare "a7"))
      occupant b (readSquare "f8") `shouldBe` (Just $ Piece Bishop Black (readSquare "f8"))
    it "returns the occupant of an empty square" $ do
      let b = toBoard initialPieces
      occupant b (readSquare "e4") `shouldBe` Nothing
      occupant b (readSquare "a3") `shouldBe` Nothing
      occupant b (readSquare "f6") `shouldBe` Nothing
      
  describe "reverseColor" $ do
    it "reverses a color" $ do
      reverseColor White `shouldBe` Black
      reverseColor Black `shouldBe` White
      