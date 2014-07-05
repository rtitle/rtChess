module Moves where
import Board

data Direction = N | S | E | W deriving (Eq, Show)
                   
data Move = Move { fromPiece :: Piece,
                   toPiece :: Piece,
                   capturedPiece :: Maybe Piece }

-- not yet implemented: castling, pawn promotion, check, checkmate
instance Show Move where
  -- pawn capture (exd4)
  show (Move from @ (Piece Pawn _ _) to (Just _)) = 
    (head . showPieceSquare $ from) : "x" ++ showPieceSquare to
    
  -- pawn move (d6)
  show (Move (Piece Pawn _ _) to Nothing) = 
    showPieceSquare to
    
  -- piece capture (Qxh2)
  show (Move _ to (Just _)) =
    show to ++ "x" ++ showPieceSquare to
    
  -- piece move (Nf3)
  show (Move _ to Nothing) =
    show to ++ showPieceSquare to

allDirections :: [Direction]
allDirections = [N, S, E, W]

rookDirections :: [[Direction]]
rookDirections = [ [a] | a <- allDirections ]

bishopDirections :: [[Direction]]
bishopDirections =[ [a,b] | a <- [N,S], b <- [E,W] ]

queenDirections :: [[Direction]]
queenDirections = rookDirections ++ bishopDirections

kingDirections :: [[Direction]]
kingDirections = rookDirections ++ bishopDirections

knightDirections :: [[Direction]]
knightDirections = [ [a,b,c] | a <- allDirections, b <- [N,S], c <- [E,W], a == b || a == c]

whitePawnDirections :: [[Direction]]
whitePawnDirections = [[N]]

whitePawnCaptureDirections :: [[Direction]]
whitePawnCaptureDirections = [[N,E], [N,W]]

blackPawnDirections :: [[Direction]]
blackPawnDirections = [[S]]

blackPawnCaptureDirections :: [[Direction]]
blackPawnCaptureDirections = [[S,E], [S,W]]

allMoves :: Board -> Pieces -> PieceColor -> [Move]
allMoves b p pc = concatMap (moves b) (filter (comparingColor pc) p)

-- not yet implemented: castling, en passant, pawn promotion
moves :: Board -> Piece -> [Move]
moves b p@(Piece pt pc sq) 
  -- rook
  | pt == Rook = map mkMove $ concatMap (genRepeatingMoves moveOrCapture) rookDirections
  
  -- bishop
  | pt == Bishop = map mkMove $ concatMap (genRepeatingMoves moveOrCapture) bishopDirections
  
  -- queen
  | pt == Queen = map mkMove $ concatMap (genRepeatingMoves moveOrCapture) queenDirections
  
  --king
  | pt == King = map mkMove $ concatMap (genSingleMove moveOrCapture) kingDirections
  
  -- knight
  | pt == Knight = map mkMove $ concatMap (genSingleMove moveOrCapture) knightDirections
  
  -- white pawn on the 2nd rank
  | (pt, pc, rank sq) == (Pawn, White, 1) =
      map mkMove $ 
        concatMap (genDoubleMove moveOnly) whitePawnDirections ++
        concatMap (genSingleMove captureOnly) whitePawnCaptureDirections
  
  -- white pawn
  | (pt, pc) == (Pawn, White) = 
      map mkMove $ 
        concatMap (genSingleMove moveOnly) whitePawnDirections ++
        concatMap (genSingleMove captureOnly) whitePawnCaptureDirections
  
  -- black pawn on the 7th rank
  | (pt, pc, rank sq) == (Pawn, Black, 6) =
      map mkMove $ 
        concatMap (genDoubleMove moveOnly) blackPawnDirections ++ 
        concatMap (genSingleMove captureOnly) blackPawnCaptureDirections
         
  -- black pawn
  | (pt, pc) == (Pawn, Black) =
      map mkMove $ 
        concatMap (genSingleMove  moveOnly) blackPawnDirections ++
        concatMap (genSingleMove captureOnly) blackPawnCaptureDirections
        
  | otherwise = []
        
  where mkMove s = Move p (Piece pt pc s) (occupant b s)
        moveOrCapture s = moveOnly s || captureOnly s
        moveOnly s = isEmptySquare b s
        captureOnly s = isCapture b pc s
        genRepeatingMoves = genMoves b sq maxBound
        genSingleMove = genMoves b sq 1
        genDoubleMove = genMoves b sq 2
        
genMoves :: Board -> Square -> Int -> (Square -> Bool) -> [Direction] -> [Square]
genMoves b sq n f d = takeWhile (canMove b d f sq). take n . drop 1 . iterate (+offsets d) $ sq

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

color :: Maybe Piece -> Maybe PieceColor
color Nothing = Nothing
color (Just (Piece _ pc _)) = Just pc
                
isCapture :: Board -> PieceColor -> Square -> Bool
isCapture b pc s = not (isEmptySquare b s) && color (occupant b s) /= Just pc

isEmptySquare :: Board -> Square -> Bool
isEmptySquare b s = occupant b s == Nothing
                    
canMove :: Board -> [Direction] -> (Square -> Bool) -> Square -> Square -> Bool
canMove b d f initSq curSq = 
  onBoards d initSq curSq &&
    (prevSq == initSq || occupant b prevSq == Nothing) &&
    f curSq
  where prevSq = curSq - offsets d