{- |
Moves.hs
Contains functionality for representing and generating piece moves on a chessboard.
-}
module Moves (
Direction(..),
Castle(..),
Move(..),
allMoves,
legalMoves,
legalMovesNotInCheck,
initialMoves,
isKingInCheck,
isCapture,
updatePieces,
updateBoard,
showMove,
readMove
) where

import Data.Maybe
import qualified Data.Vector as V
import Control.Applicative
import Board

-- |Cardinal directions which may be combined to represent different chess moves.
data Direction = N | S | E | W deriving (Eq, Show)

-- |Castling is a special-case move, since it involves moving 2 pieces, the king and the rook.
-- A castle can either be kingside or queenside.
data Castle = Kingside | Queenside deriving (Eq, Show)
             
{- |
  A Move consists of:
  - from piece, i.e. the origin piece.
  - to piece, i.e. the destination piece.
  - optional captured piece.
  - flag for whether this was an en passant capture (another special-case move).
  - optional castle type.
-}
data Move = Move { 
  fromPiece :: Piece,
  toPiece :: Piece,
  capturedPiece :: Maybe Piece,
  enPassant :: Bool,
  castle :: Maybe Castle } deriving Eq
  
-- |Show instance for Moves.
--  This is a convenience; the showMove function should generally be used instead, since
--  that takes a board context and guarantees the correct move notation.
instance Show Move where
  show m = showMove (toBoard emptyPieces) emptyPieces (pieceColor . fromPiece $ m) m

-- |All directions, useful in list comprehensions below.
allDirections :: [Direction]
allDirections = [N, S, E, W]

-- |Rooks may move north, south, east, or west.
rookDirections :: [[Direction]]
rookDirections = [ [a] | a <- allDirections ]

-- |Bishops may move northeast, northwest, southeast, or southwest.
bishopDirections :: [[Direction]]
bishopDirections =[ [a,b] | a <- [N,S], b <- [E,W] ]

-- |Queens may move like rooks or bishops.
queenDirections :: [[Direction]]
queenDirections = rookDirections ++ bishopDirections

-- |Kings may move like queens, but only one square.
kingDirections :: [[Direction]]
kingDirections = rookDirections ++ bishopDirections

-- |Knights may move 2 squares in one direction, and 1 square in another direction.
-- For a knight on the center of the board, there are 8 possible moves.
knightDirections :: [[Direction]]
knightDirections = [ [a,b,c] | a <- allDirections, b <- [N,S], c <- [E,W], a == b || a == c]

-- |White pawns can only move north.
whitePawnDirections :: [[Direction]]
whitePawnDirections = [[N]]

-- |White pawns can capture northeast or northwest.
whitePawnCaptureDirections :: [[Direction]]
whitePawnCaptureDirections = [[N,E], [N,W]]

-- |Black pawns can only move south.
blackPawnDirections :: [[Direction]]
blackPawnDirections = [[S]]

-- |Black pawns can capture southeast or southwest.
blackPawnCaptureDirections :: [[Direction]]
blackPawnCaptureDirections = [[S,E], [S,W]]

-- |The initial white moves of a chess game.
initialMoves :: [Move]
initialMoves = legalMoves (toBoard initialPieces) initialPieces White Nothing []

{- |
  Given a board, pieces, and piece color, returns all possible Moves.
  This function returns all moves physically possible on the board.  It does not test move legality, 
  such as moving pinned pieces, castling through check, prohibited castling, etc.
-}
allMoves :: Board -> Pieces -> PieceColor -> [Move]
allMoves b p pc = concatMap (moves b) (filter (\c -> pc == pieceColor c) p)

{- |
  Returns all possible _legal_ moves.
  Like allMoves, this function takes a board, pieces, and piece color.  It also takes an optional 
  en-passant capture and prohibited castles which are needed for testing move legality.
  It works by calling allMoves, and then filtering the returned list of Moves to remove illegal moves, 
  like prohibited castles and en passant legality.
-}
legalMoves ::  Board -> Pieces -> PieceColor -> Maybe Piece -> [Castle] -> [Move]
legalMoves b p pc ep prohibitedCastles = filter (liftA2 (&&) castleAllowed enPassantAllowed) $ allMoves b p pc where
  castleAllowed mv = all (\c -> not $ elem c prohibitedCastles) $ maybeToList (castle mv)
  enPassantAllowed = liftA2 (||) (not . enPassant) (\m -> ep == capturedPiece m)

{- |
  Like legalMoves, but also filters out illegal moves that result in the king being in check,
  such as moving pinned pieces.
-}
legalMovesNotInCheck :: Board -> Pieces -> PieceColor -> Maybe Piece -> [Castle] -> [Move]
legalMovesNotInCheck b p pc ep prohibitedCastles = filter notInCheck $ legalMoves b p pc ep prohibitedCastles where
  notInCheck (Move _ _ _ _ (Just cas)) = case (pc, cas) of
    (White, Kingside) -> squaresNotAttacked ["e1", "f1", "g1"]
    (Black, Kingside) -> squaresNotAttacked ["e8", "f8", "g8"]
    (White, Queenside) -> squaresNotAttacked ["e1", "d1", "c1"]
    (Black, Queenside) -> squaresNotAttacked ["e8", "d8", "c8"]
  notInCheck m = not $ isKingInCheck (updateBoard b m) (updatePieces p m) pc
  squaresNotAttacked = all (\s -> not . isSquareAttacked b p pc $ readSquare s)

-- |Given Pieces and a Move, makes the move and returns the updated Pieces.
updatePieces :: Pieces -> Move -> Pieces
updatePieces ps (Move p1 _ _ _ (Just cas)) = foldl updatePieces ps (concatCastleMoves (pieceColor p1) cas)
updatePieces ps (Move p1 p2 Nothing _ _) = p2 : filter (/=p1) ps
updatePieces ps (Move p1 p2 (Just c) _ _) = p2 : filter (liftA2 (&&) (/=p1) (/=c)) ps

-- |Given a Board and a Move, makes the move and returns the updated Board.
updateBoard :: Board -> Move -> Board
updateBoard b (Move p1 _ _ _ (Just cas)) = foldl updateBoard b (concatCastleMoves (pieceColor p1) cas)
updateBoard b (Move p1 p2 Nothing _ _) = b V.// [(square p1, Nothing), (square p2, Just p2)]
updateBoard b (Move p1 p2 (Just c) _ _) = b V.// [(square p1, Nothing), (square p2, Just p2), (square c, Just p2)]

-- |Internal move generator function.  Given a board and a piece, returns a list of possible moves.
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
        
  -- white pawn on the 5th rank (possible en passant capture)
  | (pt, pc, rank sq) == (Pawn, White, 4) = 
      (map mkMove $ 
        concatMap (genDoubleMove moveOnly) whitePawnDirections ++
        concatMap (genSingleMove captureOnly) whitePawnCaptureDirections) ++
      (map mkEnPassantMove $ 
        concatMap (genSingleMove enPassantCapture) whitePawnCaptureDirections)
  
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
        
  -- black pawn on the 4th rank (possible en passant capture)
  | (pt, pc, rank sq) == (Pawn, Black, 3) = 
      (map mkMove $ 
        concatMap (genDoubleMove moveOnly) blackPawnDirections ++
        concatMap (genSingleMove captureOnly) blackPawnCaptureDirections) ++ 
      (map mkEnPassantMove $
        concatMap (genSingleMove enPassantCapture) blackPawnCaptureDirections)
         
  -- black pawn
  | (pt, pc) == (Pawn, Black) =
      map mkMove $ 
        concatMap (genSingleMove  moveOnly) blackPawnDirections ++
        concatMap (genSingleMove captureOnly) blackPawnCaptureDirections
        
  | otherwise = []
        
  where mkMove s = Move p (Piece pt pc s) (occupant b s) False Nothing
        mkCastleMove c = Move p p Nothing False $ Just c
        mkPromotionMove s = map (\prom -> Move p (Piece prom pc s) (occupant b s) False Nothing) promotablePieces
        mkEnPassantMove s = let s' = s-8*(if pc == White then 1 else -1) in Move p (Piece pt pc s) (occupant b s') True Nothing
        moveOrCapture = liftA2 (||) moveOnly captureOnly
        moveOnly = isEmptySquare b
        captureOnly = isCapture b pc
        enPassantCapture s = let s' = s-8*(if pc == White then 1 else -1) in isCapture b pc s'
        genRepeatingMoves = genMoves b sq maxBound
        genSingleMove = genMoves b sq 1
        genDoubleMove = genMoves b sq 2

-- |Internal move generator function.     
genMoves :: Board -> Square -> Int -> (Square -> Bool) -> [Direction] -> [Square]
genMoves b sq n f d = takeWhile (canMove b d f sq). take n . drop 1 . iterate (+offsets d) $ sq

-- |Internal move generator function.
canMove :: Board -> [Direction] -> (Square -> Bool) -> Square -> Square -> Bool
canMove b d f initSq curSq = 
  onBoards initSq curSq d &&
    (prevSq == initSq || occupant b prevSq == Nothing) &&
    f curSq
  where prevSq = curSq - offsets d

-- |Given a Board and PieceColor, returns possible castles.
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
  
-- |Concats a Castle into separate King and Rook moves.
concatCastleMoves :: PieceColor -> Castle -> [Move]
concatCastleMoves pc c = [castlingKing pc c, castlingRook pc c] where
  castlingKing White Kingside = Move (Piece King White $ readSquare "e1") (Piece King White $ readSquare "g1") Nothing False Nothing
  castlingKing Black Kingside = Move (Piece King Black $ readSquare "e8") (Piece King Black $ readSquare "g8") Nothing False Nothing
  castlingKing White Queenside = Move (Piece King White $ readSquare "e1") (Piece King White $ readSquare "c1") Nothing False Nothing
  castlingKing Black Queenside = Move (Piece King Black $ readSquare "e8") (Piece King Black $ readSquare "c8") Nothing False Nothing
  castlingRook White Kingside = Move (Piece Rook White $ readSquare "h1") (Piece Rook White $ readSquare "f1") Nothing False Nothing
  castlingRook Black Kingside = Move (Piece Rook Black $ readSquare "h8") (Piece Rook Black $ readSquare "f8") Nothing False Nothing
  castlingRook White Queenside = Move (Piece Rook White $ readSquare "a1") (Piece Rook White $ readSquare "d1") Nothing False Nothing
  castlingRook Black Queenside = Move (Piece Rook Black $ readSquare "a8") (Piece Rook Black $ readSquare "d8") Nothing False Nothing

-- |Maps a Direction to a Board offset.
offset :: Direction -> Int
offset N = 8
offset E = 1
offset S = -8
offset W = -1

-- |Maps a list of Directions to a Board offset.
offsets :: [Direction] -> Int
offsets = foldl (+) 0 . map offset

-- |Takes an initial square, current square, and direction, and tests whether the square is on the board.
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

-- |Given an initial square, current square, and list of directions, tests whether the square is on the board.
onBoards :: Square -> Square -> [Direction] -> Bool
onBoards initSq curSq = foldl (&&) True . map (onBoard initSq curSq)

-- |Given a Board, PieceColor, and Square, tests whether a piece of the given color 
--  moving to the given square would be a capture.
isCapture :: Board -> PieceColor -> Square -> Bool
isCapture b pc s = Just pc == fmap (reverseColor . pieceColor) (occupant b s)

-- |Tests whether a square is empty.
isEmptySquare :: Board -> Square -> Bool
isEmptySquare b s = occupant b s == Nothing

-- |Tests whether the King of the given color is in check.
isKingInCheck :: Board -> Pieces -> PieceColor -> Bool
isKingInCheck b ps pc = Just King `elem` map (fmap pieceType . capturedPiece) (allMoves b ps $ reverseColor pc)

-- |Tests whether a piece of the given color on the given square is attacked.
isSquareAttacked :: Board -> Pieces -> PieceColor -> Square -> Bool
isSquareAttacked b ps pc sq = sq `elem` map (square . toPiece) (legalMoves b ps (reverseColor pc) Nothing [Kingside, Queenside])

-- |Shows a Move in chess algebraic notation.
showMove :: Board -> Pieces -> PieceColor -> Move -> String
showMove b ps t m@(Move from to cap ep cas)
  -- kingside castle (0-0)
  | cas == Just Kingside = "0-0" ++ checkOrCheckmate
  
  -- queenside castle (0-0-0)
  | cas == Just Queenside = "0-0-0" ++ checkOrCheckmate
  
  -- pawn capture promotion (axb1=N)
  | pieceType from == Pawn && pieceType to /= Pawn && isJust cap = (head fromPieceSquare) : "x" ++ toPieceSquare ++ "=" ++ (show . pieceType $ to) ++ checkOrCheckmate
  
  -- pawn capture en passant (exd5 e.p.)
  | pieceType from == Pawn && isJust cap && ep = (head fromPieceSquare) : "x" ++ capturedPieceSquare ++ " e.p." ++ checkOrCheckmate
  
  -- pawn capture (exd4)
  | pieceType from == Pawn && isJust cap = (head fromPieceSquare) : "x" ++ toPieceSquare ++ checkOrCheckmate
  
  -- pawn promotion (e8=Q)
  | pieceType from == Pawn && pieceType to /= Pawn = toPieceSquare ++ "=" ++ (show . pieceType $ to) ++ checkOrCheckmate
    
  -- pawn move (d6)
  | pieceType from == Pawn = toPieceSquare ++ checkOrCheckmate
     
  -- piece capture (Qxh2)
  | isJust cap = show (pieceType to) ++ rankOrFile ++ "x" ++ toPieceSquare ++ checkOrCheckmate
    
  -- piece move (Nf3)
  | otherwise = show (pieceType to) ++ rankOrFile ++ toPieceSquare ++ checkOrCheckmate
  where 
    fromPieceSquare = showSquare . square $ from
    toPieceSquare = showSquare . square $ to
    capturedPieceSquare = showSquare . square . fromJust $ cap
    disambiguate [] = ""
    disambiguate [x] = take 1 . filter (`notElem` (show . square $ x)) $ fromPieceSquare
    disambiguate _ = fromPieceSquare
    ambiguous m' = 
      (pieceType . fromPiece $ m) == (pieceType . fromPiece $ m') &&
      (pieceColor . fromPiece $ m) == (pieceColor . fromPiece $ m') &&
      (square . toPiece $ m) == (square . toPiece $ m') &&
      (square . fromPiece $ m) /= (square $ fromPiece $ m')
    rankOrFile = disambiguate . map fromPiece $ filter ambiguous (allMoves b ps t)
    checkOrCheckmate = let (updatedBoard, updatedPieces) = (updateBoard b m, updatePieces ps m) in 
      case (isKingInCheck updatedBoard updatedPieces (reverseColor t)) of 
        True -> case (legalMovesNotInCheck updatedBoard updatedPieces (reverseColor t) Nothing []) of
          [] -> "++"
          _ -> "+"
        False -> ""

-- |Reads a Move in algebraic notation.
readMove :: Board -> Pieces -> PieceColor -> Maybe Piece -> [Castle] -> String -> Maybe Move
readMove b ps t ep prohibitedCastles s = 
  let (toSq, pt, fromFileOrRank, cas) = case s of 
       -- kingside castle (0-0)
       "0-0"           -> (castleKingSq, Just King, Nothing, Just Kingside)
       
       -- queenside castle (0-0-0)
       "0-0-0"         -> (castleKingSq, Just King, Nothing, Just Queenside)
       
       -- pawn move (e4)
       r:f:[]          -> (readSquare (r:[f]), Just Pawn, Nothing, Nothing)
       
       -- piece move (Nf3)
       p:r:f:[]        -> (readSquare (r:[f]), readPieceType p, Nothing, Nothing)
       
       -- piece capture (Qxd5) or pawn capture (exf4)
       p:'x':r:f:[]    -> 
         case readPieceType p of
           Just ptype -> (readSquare (r:[f]), Just ptype, Nothing, Nothing)
           Nothing    -> (readSquare (r:[f]), Just Pawn, Just p, Nothing)
           
       -- pawn promotion (h8=Q)
       r:f:'=':prom:[] -> (readSquare (r:[f]), readPieceType prom, Nothing, Nothing)
       
       -- pawn capture promotion (axb1=N)
       p:'x':r:f:'=':prom:[] -> (readSquare (r:[f]), readPieceType prom, Just p, Nothing)
       
       -- ambiguous piece move (Nbd2)
       p:rf:r:f:[]     -> (readSquare (r:[f]), readPieceType p, Just rf, Nothing)
       
       -- ambiguous piece move capture (R3xh3)
       p:rf:'x':r:f:[] -> (readSquare (r:[f]), readPieceType p, Just rf, Nothing)
       
       -- default case
       _ -> (readSquare "a1", Nothing, Nothing, Nothing)
       
      -- filter function
      fm m = cas == (castle m) &&
             ((not . enPassant $ m) && toSq == (square . toPiece $ m) || (enPassant m && fmap square (capturedPiece m) == Just toSq)) &&
             pt == Just (pieceType . toPiece $ m) &&
             (fromFileOrRank == Nothing || any (\x -> (Just x) == fromFileOrRank) (showSquare . square . fromPiece $ m))
  
  -- apply filter function on legal moves
  in headMaybe $ filter fm (legalMoves b ps t ep prohibitedCastles)
  where headMaybe (x:[]) = Just x
        headMaybe _ = Nothing
        castleKingSq = readSquare $ if t == White then "e1" else "e8"
