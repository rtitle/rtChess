module MoveOrdering where

import Board
import Moves
import Data.Monoid

comparingMove :: Board -> Pieces -> Move -> Move -> Ordering
comparingMove b ps m1 m2 = mconcat [compareCapture b m1 m2, compareCenterMove m1 m2 ]  --, comparePawnMove m1 m2]

comparingMoveReverse :: Board -> Pieces -> Move -> Move -> Ordering
comparingMoveReverse b ps m1 m2 = reverseOrder $ (comparingMove b ps m1 m2)
  
-- |Capture moves take precedence
compareCapture :: Board -> Move -> Move -> Ordering
compareCapture b m1 m2 = (captureMove m2) `compare` (captureMove m1)
  where captureMove m = isCapture b (pieceColor . fromPiece $ m) (square . toPiece $ m)
  
-- |Pawn moves take precedence
comparePawnMove :: Move -> Move -> Ordering
comparePawnMove m1 m2 = case (pieceType . fromPiece $ m1, pieceType . fromPiece $ m2) of
  (Pawn, Pawn) -> EQ
  (Pawn, _) -> LT
  _ -> GT
  
-- |Moves toward the center take precedence
compareCenterMove :: Move -> Move -> Ordering
compareCenterMove m1 m2 = (center m2) `compare` (center m1)
  where center m = let f = file . square . toPiece $ m in f >= 2 && f <= 5

reverseOrder :: Ordering -> Ordering
reverseOrder EQ = EQ
reverseOrder LT = GT
reverseOrder GT = LT