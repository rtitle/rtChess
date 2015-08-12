module MoveOrdering where

import Board
import Moves

-- TODO: there must be a better way to do this
comparingMove :: Board -> Pieces -> Move -> Move -> Ordering
comparingMove b ps m1 m2 = 
  case (compareCapture b m1 m2) of
    EQ -> case compareCenterMove m1 m2 of
      EQ -> comparePawnMove m1 m2
      c -> c
    c-> c

-- |Check moves take precedence
-- TODO: this is expensive
compareCheck :: Board -> Pieces -> Move -> Move -> Ordering
compareCheck b ps m1 m2 = case ((checkMove b ps m1), (checkMove b ps m2)) of
  (True, False) -> GT
  (False, True) -> LT
  _ -> EQ
  
-- |Capture moves take precedence
compareCapture :: Board -> Move -> Move -> Ordering
compareCapture b m1 m2 = case ((captureMove b m1), (captureMove b m2)) of
  (True, False) -> GT
  (False, True) -> LT
  _ -> EQ
  
-- |Pawn moves take precedence
comparePawnMove :: Move -> Move -> Ordering
comparePawnMove m1 m2 = case (pieceType . fromPiece $ m1, pieceType . fromPiece $ m2) of
  (Pawn, Pawn) -> EQ
  (Pawn, _) -> GT
  _ -> LT
  
-- |Moves toward the center take precedence
compareCenterMove :: Move -> Move -> Ordering
compareCenterMove m1 m2 = case (inner . file . square . toPiece $ m1, inner . file . square . toPiece $ m2) of
  (True, False) -> GT
  (False, True) -> LT
  _ -> EQ
  where inner f = f >= 2 && f <= 5

checkMove :: Board -> Pieces -> Move -> Bool
checkMove b ps m = isKingInCheck (updateBoard b m) (updatePieces ps m) (pieceColor . fromPiece $ m)

captureMove :: Board -> Move -> Bool
captureMove b m = isCapture b (pieceColor . fromPiece $ m) (square . toPiece $ m)