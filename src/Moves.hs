module Moves where
import Board

data Direction = N | S | E | W deriving Eq
                   
-- may add additional move metadata in the future, like captures
data Move = Move { piece :: Piece }

instance Show Move where
  show (Move p) = show p ++ showPieceSquare p

allDirections = [N, S, E, W]
rookDirections = [ [a] | a <- allDirections ]
bishopDirections=[ [a,b] | a <- [N,S], b <- [E,W] ]
queenDirections = rookDirections ++ bishopDirections
kingDirections = rookDirections ++ bishopDirections
knightDirections = [ [a,b,c] | a <- allDirections, b <- [N,S], c <- [E,W],
                               (a == b) && (a /= c) || (a == c) && (a /= b) ]

allMoves :: Pieces -> [Move]
allMoves = concatMap moves

-- note: does not use board state yet
moves :: Piece -> [Move]
moves (Piece Rook color square) = map (\sq -> Move (Piece Rook color sq)) (concatMap (genRepeatingMoves square) rookDirections)
moves (Piece Bishop color square) = map (\sq -> Move (Piece Bishop color sq)) (concatMap (genRepeatingMoves square) bishopDirections)
moves (Piece Queen color square) = map (\sq -> Move (Piece Queen color sq)) (concatMap (genRepeatingMoves square) queenDirections)
moves (Piece King color square) = map (\sq -> Move (Piece King color sq)) (concatMap (genSingleMove square) kingDirections) where
moves (Piece Knight color square) = map (\sq -> Move (Piece Knight color sq)) (concatMap (genSingleMove square) knightDirections) where
-- need to add remaining pieces
moves _ = []

genRepeatingMoves :: Square -> [Direction] -> [Square]
genRepeatingMoves sq d = takeWhile (onBoards d sq) . drop 1 . iterate (+offsets d) $ sq

genSingleMove :: Square -> [Direction] -> [Square]
genSingleMove sq d = filter (onBoards d sq) [sq+offsets d]

offset :: Direction -> Int
offset N = 8
offset E = 1
offset S = -8
offset W = -1

offsets :: [Direction] -> Int
offsets dirs = foldl (\acc d -> acc + offset d) 0 dirs

onBoard :: Direction -> Square -> Square -> Bool
onBoard d initSq curSq = 
  let curRank = rank curSq
      initRank = rank initSq 
      curFile = file curSq
      initFile = file initSq
  in case d of 
    N -> curRank < 8 && curRank > initRank
    E -> curFile < 8 && curFile > initFile
    S -> curRank >= 0 && curRank < initRank
    W -> curFile >= 0 && curFile < initFile
                         
onBoards :: [Direction] -> Square -> Square -> Bool
onBoards dirs initSq curSq  = foldl (\acc d -> acc && onBoard d initSq curSq) True dirs

