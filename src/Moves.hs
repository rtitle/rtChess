module Moves where
import Board
import qualified Data.Vector as V

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

allMoves :: Board -> Pieces -> [Move]
allMoves b = concatMap (moves b)

allWhiteMoves :: Board -> Pieces -> [Move]
allWhiteMoves b = allMoves b . filter (not . black)
  
allBlackMoves :: Board -> Pieces -> [Move]
allBlackMoves b = allMoves b . filter black

-- not yet implemented: castling, pawn captures, en passant, pawn promotion
moves :: Board -> Piece -> [Move]
moves b (Piece pt pc sq) 
  | pt == Rook = 
      map mkMove $ concatMap (genRepeatingMoves b pc sq) rookDirections
  | pt == Bishop = 
      map mkMove $ concatMap (genRepeatingMoves b pc sq) bishopDirections
  | pt == Queen = 
      map mkMove $ concatMap (genRepeatingMoves b pc sq) queenDirections
  | pt == King = 
      map mkMove $ concatMap (genSingleMove b pc sq) kingDirections
  | pt == Knight =
      map mkMove $ concatMap (genSingleMove b pc sq) knightDirections
  | (pt, pc, rank sq) == (Pawn, White, 1) =
      map mkMove $ concatMap (genMoves b pc sq 2) whitePawnDirections
  | (pt, pc) == (Pawn, White) = 
      map mkMove $ concatMap (genSingleMove b pc sq) whitePawnDirections
  | (pt, pc, rank sq) == (Pawn, Black, 6) =
      map mkMove $ concatMap (genMoves b pc sq 2) blackPawnDirections
  | (pt, pc) == (Pawn, Black) =
      map mkMove $ concatMap (genSingleMove b pc sq) blackPawnDirections
  where mkMove = Move . Piece pt pc

genMoves :: Board -> PieceColor -> Square -> Int -> [Direction] -> [Square]
genMoves b pc sq n d = takeWhile (canMove b pc d sq) . take n . drop 1 . iterate (+offsets d) $ sq

genRepeatingMoves :: Board -> PieceColor -> Square -> [Direction] -> [Square]
genRepeatingMoves b pc sq d = genMoves b pc sq maxBound d

genSingleMove :: Board -> PieceColor -> Square -> [Direction] -> [Square]
genSingleMove b pc sq d = genMoves b pc sq 1 d

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

canMove :: Board -> PieceColor -> [Direction] -> Square -> Square -> Bool
canMove board color directions initSq curSq = 
  onBoards directions initSq curSq &&
    (occupant curSq == Nothing || occupant curSq /= Just color) &&
     occupant (curSq-offsets directions) == Nothing where 
  occupant sq
    | sq == initSq = Nothing
    | otherwise = case board V.! sq of
                    Nothing -> Nothing
                    Just (Piece _ color _) -> Just color