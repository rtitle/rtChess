module Board where
import Data.Char
import Data.List
import Data.Bits
import qualified Data.Vector as V
import Utils

type Square = Int
data PieceType = Rook | Knight | Bishop | King | Queen | Pawn deriving Eq
data PieceColor = Black | White deriving (Eq, Show)
data Piece = Piece { 
  pieceType :: PieceType, 
  pieceColor :: PieceColor,
  square :: Square } deriving Eq
  
-- we use both a list of pieces and a vector of squares to represent the board
type Pieces = [Piece]
type Board = V.Vector (Maybe Piece)

instance Show PieceType where
  show King = "K"
  show Queen = "Q"
  show Knight = "N"
  show Rook = "R"
  show Bishop = "B"
  show Pawn = "P"
  
instance Show Piece where
  -- black pieces are displayed as lowercase
  show (Piece pt Black _) = map toLower (show pt)
  show (Piece pt White _) = show pt

readPieceType :: Char -> Maybe PieceType
readPieceType 'K' = Just King
readPieceType 'Q' = Just Queen
readPieceType 'N' = Just Knight
readPieceType 'R' = Just Rook
readPieceType 'B' = Just Bishop
readPieceType 'P' = Just Pawn
readPieceType _ = Nothing

showSquare :: Square -> String
showSquare s = f:r where
  f = chr . (+ ord 'a') . file $ s
  r = show . succ . rank $ s

readSquare :: String -> Square
readSquare [] = -1
readSquare (f:r) 
  | isNumber . head $ r = ord f - ord 'a' + 
                          ((*8) . pred . read $ r)
  | otherwise = -1

prettyPieces :: Pieces -> String
prettyPieces = prettyBoard . toBoard
  
prettyBoard :: Board -> String
prettyBoard = unlines . 
             (hline:) . 
             concatMap (:[hline]) . 
             reverse . 
             map (++vline) . 
             map concat . 
             splitEvery 8 . 
             map (vline++) .
             map prettySquare .
             V.toList where
  hline = surround vline (concat . replicate 31 $ "-")
  vline = "|"
  prettySquare Nothing = "   "
  prettySquare (Just p) = surround " " (show p)

toBoard :: Pieces -> Board
toBoard [] = V.fromList $ replicate 64 Nothing
toBoard ps = V.fromList $ pad (-1) $ foldr combine (64, []) sortedPieces where
  combine p@Piece {square = sq} acc = (sq, Just p:pad sq acc)
  pad curSq (lastSq, acc) =  replicate (lastSq-curSq-1) Nothing ++ acc
  sortedPieces = sortBy comparingSquare ps

comparingSquare :: Piece -> Piece -> Ordering
comparingSquare p1 p2 = compare (square p1) (square p2)
  
file :: Int -> Int
file n = n .&. 7

rank :: Int -> Int
rank n = n `shiftR` 3

occupant :: Board -> Square -> Maybe Piece
occupant b sq = b V.! sq

reverseColor :: PieceColor -> PieceColor
reverseColor White = Black
reverseColor _ = White

-- ** some boards **

initialPieces :: Pieces
initialPieces = [
  Piece Rook White   (readSquare "a1"),
  Piece Knight White (readSquare "b1"),
  Piece Bishop White (readSquare "c1"),
  Piece Queen White  (readSquare "d1"),
  Piece King White   (readSquare "e1"),
  Piece Bishop White (readSquare "f1"),
  Piece Knight White (readSquare "g1"),
  Piece Rook White   (readSquare "h1"),
  Piece Pawn White   (readSquare "a2"),
  Piece Pawn White   (readSquare "b2"),
  Piece Pawn White   (readSquare "c2"),
  Piece Pawn White   (readSquare "d2"),
  Piece Pawn White   (readSquare "e2"),
  Piece Pawn White   (readSquare "f2"),
  Piece Pawn White   (readSquare "g2"),
  Piece Pawn White   (readSquare "h2"),
  Piece Pawn Black   (readSquare "a7"),
  Piece Pawn Black   (readSquare "b7"),
  Piece Pawn Black   (readSquare "c7"),
  Piece Pawn Black   (readSquare "d7"),
  Piece Pawn Black   (readSquare "e7"),
  Piece Pawn Black   (readSquare "f7"),
  Piece Pawn Black   (readSquare "g7"),
  Piece Pawn Black   (readSquare "h7"),
  Piece Rook Black   (readSquare "a8"),
  Piece Knight Black (readSquare "b8"),
  Piece Bishop Black (readSquare "c8"),
  Piece Queen Black  (readSquare "d8"),
  Piece King Black   (readSquare "e8"),
  Piece Bishop Black (readSquare "f8"),
  Piece Knight Black (readSquare "g8"),
  Piece Rook Black   (readSquare "h8")]
  
emptyPieces :: Pieces
emptyPieces = []

mateIn1 :: Pieces
mateIn1 = [
  Piece Rook White (readSquare "b1"),
  Piece Rook Black (readSquare "a5"),
  Piece King White (readSquare "g1"),
  Piece Pawn White (readSquare "h3"),
  Piece Pawn White (readSquare "g2"),
  Piece Pawn White (readSquare "f2"),
  Piece King Black (readSquare "g8"),
  Piece Pawn Black (readSquare "h7"),
  Piece Pawn Black (readSquare "g7"),
  Piece Pawn Black (readSquare "f7")]

sicilianDragon :: Pieces
sicilianDragon = [
  Piece Pawn White   (readSquare "a2"),
  Piece Pawn White   (readSquare "b2"), 
  Piece Pawn White   (readSquare "c2"), 
  Piece Pawn White   (readSquare "e4"), 
  Piece Pawn White   (readSquare "f3"), 
  Piece Pawn White   (readSquare "g2"), 
  Piece Pawn White   (readSquare "h2"), 
  Piece Knight White (readSquare "c3"),
  Piece Knight White (readSquare "d4"),
  Piece Bishop White (readSquare "c4"),
  Piece Bishop White (readSquare "e3"),
  Piece Rook White   (readSquare "a1"),
  Piece Rook White   (readSquare "h1"),
  Piece Queen White  (readSquare "d2"),
  Piece King White   (readSquare "e1"),
  Piece Pawn Black   (readSquare "a7"),
  Piece Pawn Black   (readSquare "b7"), 
  Piece Pawn Black   (readSquare "d6"), 
  Piece Pawn Black   (readSquare "e7"), 
  Piece Pawn Black   (readSquare "f7"), 
  Piece Pawn Black   (readSquare "g6"), 
  Piece Pawn Black   (readSquare "h7"), 
  Piece Knight Black (readSquare "c6"),
  Piece Knight Black (readSquare "f6"),
  Piece Bishop Black (readSquare "c8"),
  Piece Bishop Black (readSquare "g7"),
  Piece Rook Black   (readSquare "a8"),
  Piece Rook Black   (readSquare "f8"),
  Piece Queen Black  (readSquare "d8"),
  Piece King Black   (readSquare "g8")]
