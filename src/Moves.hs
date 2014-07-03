module Moves where
import Board

data Direction = N | S | E | W deriving (Eq, Show)
                   
-- may add additional move metadata in the future, like captures
data Move = Move { piece :: Piece }

instance Show Move where
  show (Move (Piece Pawn color square)) = showPieceSquare (Piece Pawn color square)
  show (Move p) = show p ++ showPieceSquare p

allDirections = [N, S, E, W]
rookDirections = [ [a] | a <- allDirections ]
bishopDirections =[ [a,b] | a <- [N,S], b <- [E,W] ]
queenDirections = rookDirections ++ bishopDirections
kingDirections = rookDirections ++ bishopDirections
knightDirections = [ [a,b,c] | a <- allDirections, b <- [N,S], c <- [E,W], a == b || a == c]
whitePawnDirections = [[N]] --TODO: add captures, [N,E], [N,W]]
blackPawnDirections = [[S]] --TODO: add captures, [S,E], [S,W]]

allMoves :: Pieces -> [Move]
allMoves = concatMap moves

allWhiteMoves :: Pieces -> [Move]
allWhiteMoves = allMoves . filter (not . black)
  
allBlackMoves :: Pieces -> [Move]
allBlackMoves = allMoves . filter black

-- note: does not use board state yet
-- no castling, pawn promotion, captures, en passant, etc yet
moves :: Piece -> [Move]
moves (Piece pt pc sq) 
  | pt == Rook = 
      map mkMove $ concatMap (genRepeatingMoves sq) rookDirections
  | pt == Bishop = 
      map mkMove $ concatMap (genRepeatingMoves sq) bishopDirections
  | pt == Queen = 
      map mkMove $ concatMap (genRepeatingMoves sq) queenDirections
  | pt == King = 
      map mkMove $ concatMap (genSingleMove sq) kingDirections
  | pt == Knight =
      map mkMove $ concatMap (genSingleMove sq) knightDirections
  | (pt, pc, rank sq) == (Pawn, White, 1) =
      map mkMove $ concatMap (genMoves sq 2) whitePawnDirections
  | (pt, pc) == (Pawn, White) = 
      map mkMove $ concatMap (genSingleMove sq) whitePawnDirections
  | (pt, pc, rank sq) == (Pawn, Black, 6) =
      map mkMove $ concatMap (genMoves sq 2) blackPawnDirections
  | (pt, pc) == (Pawn, Black) =
      map mkMove $ concatMap (genSingleMove sq) blackPawnDirections
  where mkMove = Move . Piece pt pc

genMoves :: Square -> Int -> [Direction] -> [Square]
genMoves sq n d = takeWhile (onBoards d sq) . take n . drop 1 . iterate (+offsets d) $ sq

genRepeatingMoves :: Square -> [Direction] -> [Square]
genRepeatingMoves sq d = genMoves sq maxBound d

genSingleMove :: Square -> [Direction] -> [Square]
genSingleMove sq d = genMoves sq 1 d

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

