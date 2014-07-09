module Moves where
import Data.Maybe
import Control.Applicative
import Board

data Direction = N | S | E | W deriving (Eq, Show)
                   
data Move = Move { fromPiece :: Piece,
                   toPiece :: Piece,
                   capturedPiece :: Maybe Piece }

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

initialMoves :: [Move]
initialMoves = allMoves (toBoard initialPieces) initialPieces White

allMoves :: Board -> Pieces -> PieceColor -> [Move]
allMoves b p pc = concatMap (moves b) (filter (\c -> pc == pieceColor c) p)

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
        moveOrCapture = liftA2 (||) moveOnly captureOnly
        moveOnly = isEmptySquare b
        captureOnly = isCapture b pc
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
offsets = foldl (+) 0 . map offset

onBoard :: Square -> Square -> Direction -> Bool
onBoard initSq curSq d = 
  let curRank = rank curSq
      initRank = rank initSq 
      curFile = file curSq
      initFile = file initSq
  in case d of 
    N -> curRank < 8 && curRank > initRank
    E -> curFile < 8 && curFile > initFile
    S -> curRank >= 0 && curRank < initRank
    W -> curFile >= 0 && curFile < initFile
                         
onBoards :: Square -> Square -> [Direction] -> Bool
onBoards initSq curSq = foldl (&&) True . map (onBoard initSq curSq)
                
isCapture :: Board -> PieceColor -> Square -> Bool
isCapture b pc s = Just pc == fmap (reverseColor . pieceColor) (occupant b s)

isEmptySquare :: Board -> Square -> Bool
isEmptySquare b s = occupant b s == Nothing

canMove :: Board -> [Direction] -> (Square -> Bool) -> Square -> Square -> Bool
canMove b d f initSq curSq = 
  onBoards initSq curSq d &&
    (prevSq == initSq || occupant b prevSq == Nothing) &&
    f curSq
  where prevSq = curSq - offsets d
  
showMove :: Board -> Pieces -> PieceColor -> Move -> String
showMove b ps t m@(Move from to cap)
  -- pawn capture (exd4)
  | pieceType from == Pawn && isJust cap = (head fromPieceSquare) : "x" ++ toPieceSquare
    
  -- pawn move (d6)
  | pieceType from == Pawn = toPieceSquare
     
  -- piece capture (Qxh2)
  | isJust cap = show (pieceType to) ++ rankOrFile ++ "x" ++ toPieceSquare
    
  -- piece move (Nf3)
  | otherwise = show (pieceType to) ++ rankOrFile ++ toPieceSquare
  where 
    fromPieceSquare = showSquare . square $ from
    toPieceSquare = showSquare . square $ to
    disambiguate [] = ""
    disambiguate [x] = take 1 . filter (`notElem` (show . square $ x)) $ fromPieceSquare
    disambiguate _ = fromPieceSquare
    ambiguous m' = 
      (pieceType . fromPiece $ m) == (pieceType . fromPiece $ m') &&
      (pieceColor . fromPiece $ m) == (pieceColor . fromPiece $ m') &&
      (square . toPiece $ m) == (square . toPiece $ m') &&
      (square . fromPiece $ m) /= (square $ fromPiece $ m')
    rankOrFile = disambiguate . map fromPiece $ filter ambiguous (allMoves b ps t)
               
readMove :: Board -> Pieces -> PieceColor -> String -> Maybe Move
readMove b ps t s = 
  let (toSq, pt, fromFileOrRank) = case s of 
       r:f:[] -> (readSquare (r:[f]), Just Pawn, Nothing)
       p:r:f:[] -> (readSquare (r:[f]), readPieceType p, Nothing)
       p:'x':r:f:[] -> case readPieceType p of
                         Just ptype -> (readSquare (r:[f]), Just ptype, Nothing)
                         Nothing -> (readSquare (r:[f]), Just Pawn, Just p)
       p:rf:r:f:[] -> (readSquare (r:[f]), readPieceType p, Just rf)
       p:rf:'x':r:f:[] -> (readSquare (r:[f]), readPieceType p, Just rf)
       _ -> (readSquare "a1", Nothing, Nothing)
      fm m = toSq == (square . toPiece $ m) &&
             pt == Just (pieceType . toPiece $ m) &&
             (fromFileOrRank == Nothing || any (\x -> (Just x) == fromFileOrRank) (showSquare . square . fromPiece $ m))
  in headMaybe $ filter fm (allMoves b ps t)
  where headMaybe (x:[]) = Just x
        headMaybe _ = Nothing
