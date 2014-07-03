module Moves where
import Board

data Direction = N | NE | E | SE | S | SW | W | NW
-- may add additional move metadata in the future, like captures
data Move = Move { piece :: Piece }

instance Show Move where
  show (Move p) = show p ++ showPieceSquare p

rookDirections = [N, S, E, W]
bishopDirections = [NE, SE, NW, SW]
queenDirections = rookDirections ++ bishopDirections
kingDirections = rookDirections ++ bishopDirections

allMoves :: Pieces -> [Move]
allMoves = concatMap moves

-- note: does not use board state yet
moves :: Piece -> [Move]
moves (Piece Rook color square) = map (\sq -> Move (Piece Rook color sq)) (concatMap (genMoves square) rookDirections)
moves (Piece Bishop color square) = map (\sq -> Move (Piece Bishop color sq)) (concatMap (genMoves square) bishopDirections)
moves (Piece Queen color square) = map (\sq -> Move (Piece Queen color sq)) (concatMap (genMoves square) queenDirections)
moves (Piece King color square) = map (\sq -> Move (Piece King color sq)) (concatMap (genKingMoves square) kingDirections) where
  genKingMoves sq d = filter (onBoard d sq) [sq+offset d]
-- need to add remaining pieces
moves _ = []

genMoves :: Square -> Direction -> [Square]
genMoves sq d = takeWhile (onBoard d sq) . drop 1 . iterate (+offset d) $ sq

offset :: Direction -> Int
offset N = 8
offset NE = 9
offset E = 1
offset SE = -7
offset S = -8
offset SW = -9
offset W = -1
offset NW = 7

onBoard :: Direction -> Square -> Square -> Bool
onBoard N initSq curSq = let curRank = rank curSq
                             initRank = rank initSq 
                         in curRank < 8 && curRank > initRank
onBoard E initSq curSq = let curFile = file curSq
                             initFile = file initSq 
                         in curFile < 8 && curFile > initFile
onBoard S initSq curSq = let curRank = rank curSq
                             initRank = rank initSq 
                         in curRank >= 0 && curRank < initRank
onBoard W initSq curSq = let curFile = file curSq
                             initFile = file initSq 
                         in curFile >= 0 && curFile < initFile
onBoard NE initSq curSq = onBoard N initSq curSq && onBoard E initSq curSq
onBoard NW initSq curSq = onBoard N initSq curSq && onBoard W initSq curSq
onBoard SE initSq curSq = onBoard S initSq curSq && onBoard E initSq curSq
onBoard SW initSq curSq = onBoard S initSq curSq && onBoard W initSq curSq 