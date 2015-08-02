module Eval where
import Board

infinity = 1000 :: Int

valuePiece :: PieceType -> Int
valuePiece Pawn = 1
valuePiece Rook = 5
valuePiece Knight = 3
valuePiece Bishop = 3
valuePiece Queen = 9
valuePiece King = infinity

evalPieces :: Pieces -> (Piece -> Bool) -> Int
evalPieces ps pred = foldl addValue 0 (filter pred ps)
  where addValue acc p = acc + (valuePiece . pieceType $ p)
        
evalColor :: Pieces -> PieceColor -> Int
evalColor ps pc = evalPieces ps (\p -> pieceColor p == pc)

evalBoard :: Pieces -> Int
evalBoard ps = evalPieces ps (\p -> True)