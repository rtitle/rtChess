{- |
MovesSpec.hs
Spec for the Moves module.
-}
module MovesSpec (
main, 
spec
) where

import Test.Hspec
import Board
import Moves

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

expectedWhiteInitialMoves :: [String]
expectedWhiteInitialMoves = ["Nc3", "Na3", "Nh3", "Nf3"] ++ [a:b:[] | a <- ['a'..'h'], b <- ['3'..'4']]

expectedBlackInitialMoves :: [String]
expectedBlackInitialMoves = ["Nc6", "Na6", "Nh6", "Nf6"] ++ [a:b:[] | a <- ['a'..'h'], b <- reverse ['5'..'6']]

allMovesHelper :: Pieces -> PieceColor -> [String]
allMovesHelper p c = let b = toBoard p in map (showMove b p c) (allMoves b p c)

spec :: Spec
spec = do
  describe "allMoves" $ do
    it "returns all valid moves for a position" $ do
      allMovesHelper initialPieces White `shouldBe` expectedWhiteInitialMoves
      allMovesHelper initialPieces Black `shouldBe` expectedBlackInitialMoves
      allMovesHelper sicilianDragon White `shouldSatisfy` (\m -> all (`elem` m) ["Ke2", "Kf1", "Kd1", "Qd1", "Qf2", "Rg1", "Rf1", "Bf4", "Bh6", "Bb3", "Nd5", "Na4", "Nb3", "a3", "h4", "e5"])
      allMovesHelper sicilianDragon White `shouldSatisfy` (\m -> all (`notElem` m) ["Ke8", "Rr8", "d1", "Bb8"])
      allMovesHelper sicilianDragon Black `shouldSatisfy` (\m -> all (`elem` m) ["Kh8", "Qc7", "Rb8", "Bd7", "Bh3", "Ne5", "Nd5", "d5", "e5", "h6"])
      allMovesHelper sicilianDragon Black `shouldSatisfy` (\m -> all (`notElem` m) ["Ke8", "Rr8", "d1", "Bb8"])
      
    it "returns capture moves" $ do
      pending
    it "allows leaving the king in check" $ do
      pending
    it "returns castle moves" $ do
      pending
    it "allows castling through check" $ do
      pending
    it "returns en passant moves" $ do
      pending
    it "returns moves in sorted order" $ do
      pending
      
  describe "legalMoves" $ do
    it "returns all valid moves for a position" $ do
      pending
    it "returns capture moves" $ do
      pending
    it "does not allow leaving the king in check" $ do
      pending
    it "returns castle moves" $ do
      pending
    it "does not allow castling through check" $ do
      pending
    it "does not return prohibited castles" $ do
      pending
    it "returns en passant moves" $ do
      pending
    it "does not return illegal en passant moves" $ do
      pending
    it "returns moves in sorted order" $ do
      pending
      
  describe "initialMoves" $ do
    it "returns the legal moves from the initial position" $ do
      pending
  
  describe "updatePieces" $ do
    it "makes a move and returns the updated pieces" $ do
      pending
    it "makes a capture move and returns the updated pieces" $ do
      pending
    it "makes a castle move and returns the updated pieces" $ do
      pending
    it "returns the same pieces if given an invalid move" $ do
      pending
      
  describe "updateBoard" $ do
    it "makes a move and returns the updated board" $ do
      pending
    it "makes a capture move and returns the updated board" $ do
      pending
    it "makes a castle move and returns the updated board" $ do
      pending
    it "returns the same board if given an invalid move" $ do
      pending
        
  describe "showMove" $ do
    it "shows kingside castling" $ do
      pending
    it "shows queenside castling" $ do 
      pending
    it "shows pawn capture promotion" $ do
      pending
    it "shows pawn capture en passant" $ do
      pending
    it "shows pawn capture" $ do 
      pending
    it "shows pawn promotion" $ do
      pending
    it "shows pawn moves" $ do
      pending
    it "shows piece captures" $ do
      pending
    it "shows piece moves" $ do
      pending
    it "disambiguates based on file" $ do
      pending
    it "disambiguates based on rank" $ do
      pending
    it "disambiguates captures" $ do
      pending
    it "displays check" $ do
      pendingWith "check notation not yet implemented"
    it "displays checkmate" $ do
      pendingWith "checkmate notation not yet implemented"
      
  describe "readMove" $ do
    it "reads kingside castling" $ do
      pending
    it "reads queenside castling" $ do 
      pending
    it "reads pawn capture promotion" $ do
      pending
    it "reads pawn capture en passant" $ do
      pending
    it "reads pawn capture" $ do 
      pending
    it "reads pawn promotion" $ do
      pending
    it "reads pawn moves" $ do
      pending
    it "reads piece captures" $ do
      pending
    it "reads piece moves" $ do
      pending
    it "disambiguates based on file" $ do
      pending
    it "disambiguates based on rank" $ do
      pending
    it "disambiguates captures" $ do
      pending
    it "reads check moves" $ do
      pendingWith "check notation yet implemented"
    it "reads checkmate moves" $ do
      pendingWith "checkmate notation yet implemented"
    it "does not read illegal moves" $ do
      pending
    it "does not read illegal castles" $ do
      pending
    it "does not read castles through check" $ do
      pending
    it "does not read illegal en passant moves" $ do
      pending
    it "does not read moves that result in the king being in check" $ do
      pending
    it "does not read moves from invalid strings" $ do 
      pending
    
    