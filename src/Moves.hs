module Moves where
import Data.Maybe
import Data.List
import qualified Data.Vector as V
import Control.Applicative
import Board

data Direction = N | S | E | W deriving (Eq, Show)

data Castle = Kingside | Queenside deriving Eq
                   
data Move = Move { 
  fromPiece :: Piece,
  toPiece :: Piece,
  capturedPiece :: Maybe Piece,
  castle :: Maybe Castle }

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
initialMoves = legalMoves (toBoard initialPieces) initialPieces White []

allMoves :: Board -> Pieces -> PieceColor -> [Move]
allMoves b p pc = sortBy comparingMove $ concatMap (moves b) (filter (\c -> pc == pieceColor c) p)

legalMoves :: Board -> Pieces -> PieceColor -> [Castle] -> [Move]
legalMoves b p pc prohibitedCastles = filter (liftA2 (&&) notInCheck castleAllowed) $ allMoves b p pc where
  notInCheck (Move _ _ _ (Just cas)) = case (pc, cas) of
    (White, Kingside) -> squaresNotAttacked ["e1", "f1", "g1"]
    (Black, Kingside) -> squaresNotAttacked ["e8", "f8", "g8"]
    (White, Queenside) -> squaresNotAttacked ["e1", "d1", "c1"]
    (Black, Queenside) -> squaresNotAttacked ["e8", "d8", "c8"]
  notInCheck m = not $ isKingInCheck (updateBoard b m) (updatePieces p m) pc
  squaresNotAttacked = all (\s -> not . isSquareAttacked b p pc $ readSquare s)
  castleAllowed mv = all (\c -> not $ elem c prohibitedCastles) $ maybeToList (castle mv)
  
updatePieces :: Pieces -> Move -> Pieces
updatePieces ps (Move p1 _ _ (Just cas)) = foldl updatePieces ps (concatCastleMoves (pieceColor p1) cas)
updatePieces ps (Move p1 p2 Nothing _) = p2 : filter (/=p1) ps
updatePieces ps (Move p1 p2 (Just c) _) = p2 : filter (liftA2 (&&) (/=p1) (/=c)) ps

updateBoard :: Board -> Move -> Board
updateBoard b (Move p1 _ _ (Just cas)) = foldl updateBoard b (concatCastleMoves (pieceColor p1) cas)
updateBoard b (Move p1 p2 Nothing _) = b V.// [(square p1, Nothing), (square p2, Just p2)]
updateBoard b (Move p1 p2 (Just c) _) = b V.// [(square p1, Nothing), (square p2, Just p2), (square c, Just p2)]

-- not yet implemented: en passant
moves :: Board -> Piece -> [Move]
moves b p@(Piece pt pc sq) 
  -- rook
  | pt == Rook = map mkMove $ concatMap (genRepeatingMoves moveOrCapture) rookDirections
  
  -- bishop
  | pt == Bishop = map mkMove $ concatMap (genRepeatingMoves moveOrCapture) bishopDirections
  
  -- queen
  | pt == Queen = map mkMove $ concatMap (genRepeatingMoves moveOrCapture) queenDirections
  
  --king
  | pt == King = 
      (map mkMove $ concatMap (genSingleMove moveOrCapture) kingDirections) ++ 
      (map mkCastleMove $ castles b pc)
  
  -- knight
  | pt == Knight = map mkMove $ concatMap (genSingleMove moveOrCapture) knightDirections
  
  -- white pawn on the 2nd rank
  | (pt, pc, rank sq) == (Pawn, White, 1) =
      map mkMove $ 
        concatMap (genDoubleMove moveOnly) whitePawnDirections ++
        concatMap (genSingleMove captureOnly) whitePawnCaptureDirections
        
  -- white pawn on the 7th rank
  | (pt, pc, rank sq) == (Pawn, White, 6) =
      concatMap mkPromotionMove $ 
        concatMap (genSingleMove moveOnly) whitePawnDirections ++
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
        
  -- black pawn on the 2nd rank
  | (pt, pc, rank sq) == (Pawn, Black, 1) =
      concatMap mkPromotionMove $ 
        concatMap (genSingleMove moveOnly) blackPawnDirections ++
        concatMap (genSingleMove captureOnly) blackPawnCaptureDirections
         
  -- black pawn
  | (pt, pc) == (Pawn, Black) =
      map mkMove $ 
        concatMap (genSingleMove  moveOnly) blackPawnDirections ++
        concatMap (genSingleMove captureOnly) blackPawnCaptureDirections
        
  | otherwise = []
        
  where mkMove s = Move p (Piece pt pc s) (occupant b s) Nothing
        mkCastleMove c = Move p p Nothing $ Just c
        mkPromotionMove s = map (\prom -> Move p (Piece prom pc s) (occupant b s) Nothing) promotablePieces
        moveOrCapture = liftA2 (||) moveOnly captureOnly
        moveOnly = isEmptySquare b
        captureOnly = isCapture b pc
        genRepeatingMoves = genMoves b sq maxBound
        genSingleMove = genMoves b sq 1
        genDoubleMove = genMoves b sq 2
        
genMoves :: Board -> Square -> Int -> (Square -> Bool) -> [Direction] -> [Square]
genMoves b sq n f d = takeWhile (canMove b d f sq). take n . drop 1 . iterate (+offsets d) $ sq

castles :: Board -> PieceColor -> [Castle]
castles b t = filter f [Kingside, Queenside] where
  f c = case (c,t) of
    (Kingside, White) -> rook "h1" && emptySq "g1" && emptySq "f1" && king "e1" 
    (Kingside, Black) -> rook "h8" && emptySq "g8" && emptySq "f8" && king "e8" 
    (Queenside, White) -> rook "a1" && emptySq "b1" && emptySq "c1" && emptySq "d1" && king "e1" 
    (Queenside, Black) -> rook "a8" && emptySq "b8" && emptySq "c8" && emptySq "d8" && king "e8" 
  rook sq = let s = readSquare sq in occupant b s == Just (Piece Rook t s)
  king sq = let s = readSquare sq in occupant b s == Just (Piece King t s)
  emptySq sq = let s = readSquare sq in occupant b s == Nothing
  
concatCastleMoves :: PieceColor -> Castle -> [Move]
concatCastleMoves pc c = [castlingKing pc c, castlingRook pc c] where
  castlingKing White Kingside = Move (Piece King White $ readSquare "e1") (Piece King White $ readSquare "g1") Nothing Nothing
  castlingKing Black Kingside = Move (Piece King Black $ readSquare "e8") (Piece King Black $ readSquare "g8") Nothing Nothing
  castlingKing White Queenside = Move (Piece King White $ readSquare "e1") (Piece King White $ readSquare "c1") Nothing Nothing
  castlingKing Black Queenside = Move (Piece King Black $ readSquare "e8") (Piece King Black $ readSquare "c8") Nothing Nothing
  castlingRook White Kingside = Move (Piece Rook White $ readSquare "h1") (Piece Rook White $ readSquare "f1") Nothing Nothing
  castlingRook Black Kingside = Move (Piece Rook Black $ readSquare "h8") (Piece Rook Black $ readSquare "f8") Nothing Nothing
  castlingRook White Queenside = Move (Piece Rook White $ readSquare "a1") (Piece Rook White $ readSquare "d1") Nothing Nothing
  castlingRook Black Queenside = Move (Piece Rook Black $ readSquare "a8") (Piece Rook Black $ readSquare "d8") Nothing Nothing

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
  
isKingInCheck :: Board -> Pieces -> PieceColor -> Bool
isKingInCheck b ps pc = Just King `elem` map (fmap pieceType . capturedPiece) (allMoves b ps $ reverseColor pc)

isSquareAttacked :: Board -> Pieces -> PieceColor -> Square -> Bool
isSquareAttacked b ps pc sq = sq `elem` map (square . toPiece) (legalMoves b ps (reverseColor pc) [])

canMove :: Board -> [Direction] -> (Square -> Bool) -> Square -> Square -> Bool
canMove b d f initSq curSq = 
  onBoards initSq curSq d &&
    (prevSq == initSq || occupant b prevSq == Nothing) &&
    f curSq
  where prevSq = curSq - offsets d
  
showMove :: Board -> Pieces -> PieceColor -> Move -> String
showMove b ps t m@(Move from to cap cas)
  -- kingside castle (0-0)
  | cas == Just Kingside = "0-0"
  
  -- queenside castle (0-0-0)
  | cas == Just Queenside = "0-0-0"
  
  -- pawn capture promotion (axb1=N)
  | pieceType from == Pawn && pieceType to /= Pawn && isJust cap = (head fromPieceSquare) : "x" ++ toPieceSquare ++ "=" ++ (show . pieceType $ to)
  
  -- pawn capture (exd4)
  | pieceType from == Pawn && isJust cap = (head fromPieceSquare) : "x" ++ toPieceSquare
  
  -- pawn promotion (e8=Q)
  | pieceType from == Pawn && pieceType to /= Pawn = toPieceSquare ++ "=" ++ (show . pieceType $ to)
    
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
               
readMove :: Board -> Pieces -> PieceColor -> [Castle] -> String -> Maybe Move
readMove b ps t prohibitedCastles s = 
  let (toSq, pt, fromFileOrRank, cas) = case s of 
       "0-0" -> (castleKingSq, Just King, Nothing, Just Kingside)
       "0-0-0" -> (castleKingSq, Just King, Nothing, Just Queenside)
       r:f:[] -> (readSquare (r:[f]), Just Pawn, Nothing, Nothing)
       p:r:f:[] -> (readSquare (r:[f]), readPieceType p, Nothing, Nothing)
       p:'x':r:f:[] -> case readPieceType p of
                         Just ptype -> (readSquare (r:[f]), Just ptype, Nothing, Nothing)
                         Nothing -> (readSquare (r:[f]), Just Pawn, Just p, Nothing)
       r:f:'=':prom:[] -> (readSquare (r:[f]), readPieceType prom, Nothing, Nothing)
       p:'x':r:f:'=':prom:[] -> (readSquare (r:[f]), readPieceType prom, Just p, Nothing)
       p:rf:r:f:[] -> (readSquare (r:[f]), readPieceType p, Just rf, Nothing)
       p:rf:'x':r:f:[] -> (readSquare (r:[f]), readPieceType p, Just rf, Nothing)
       _ -> (readSquare "a1", Nothing, Nothing, Nothing)
      fm m = cas == (castle m) &&
             toSq == (square . toPiece $ m) &&
             pt == Just (pieceType . toPiece $ m) &&
             (fromFileOrRank == Nothing || any (\x -> (Just x) == fromFileOrRank) (showSquare . square . fromPiece $ m))
  in headMaybe $ filter fm (legalMoves b ps t prohibitedCastles)
  where headMaybe (x:[]) = Just x
        headMaybe _ = Nothing
        castleKingSq = readSquare $ if t == White then "e1" else "e8"
        
comparingMove :: Move -> Move -> Ordering
comparingMove p1 p2 
  | comparePieceType == EQ = comparePieceSquare
  | otherwise = comparePieceType
  where 
    comparePieceType = compare (pieceType . fromPiece $ p1) (pieceType . fromPiece $ p2)
    comparePieceSquare = compare (square . fromPiece $ p1) (square . fromPiece $ p2)
