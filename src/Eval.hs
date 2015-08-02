module Eval where
import Board

infinity :: Int
infinity = 1000

threshold :: Int
threshold = 900

valuePieceType :: PieceType -> Int
valuePieceType Pawn = 1
valuePieceType Rook = 5
valuePieceType Knight = 3
valuePieceType Bishop = 3
valuePieceType Queen = 9
valuePieceType King = infinity

valuePiece :: Piece -> Int
valuePiece (Piece pt White _) = valuePieceType pt
valuePiece (Piece pt Black _) = negate . valuePieceType $ pt

evalPieces :: Pieces -> (Piece -> Bool) -> Int
evalPieces ps f = foldl addValue 0 (filter f ps)
  where addValue acc p = acc + (valuePiece p)
        
evalColor :: Pieces -> PieceColor -> Int
evalColor ps pc = evalPieces ps (\p -> pieceColor p == pc)

evalBoard :: Pieces -> Int
evalBoard ps = evalPieces ps (\_ -> True)