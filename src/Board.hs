{- |
Board.hs
Contains functionality for representing a chessboard and pieces.
-}
module Board where
import Data.Bits
import Data.Char
import Data.List
import qualified Data.Vector as V
import Utils

{- |
  A Square is represented by an integer 0..63.
  The board is oriented such that a1 is square 0, b1 is square 1, ..., and h8 is square 63.
-}
type Square = Int

-- |Types of chess pieces.
data PieceType = King | Queen | Rook | Bishop | Knight | Pawn deriving (Eq, Ord)

instance Show PieceType where
  show King = "K"
  show Queen = "Q"
  show Knight = "N"
  show Rook = "R"
  show Bishop = "B"
  show Pawn = "P"

-- |Piece color is either white or black.
data PieceColor = Black | White deriving (Eq, Show)

-- |A Piece data structure consists of a piece type, piece color, and square.
data Piece = Piece { 
  pieceType :: PieceType, 
  pieceColor :: PieceColor,
  square :: Square } deriving Eq
  
instance Show Piece where
  -- black pieces are displayed as lowercase
  show (Piece pt Black _) = map toLower (show pt)
  show (Piece pt White _) = show pt
  
-- |Representation of the board as a List of Pieces.
type Pieces = [Piece]

-- |Representation of the board as a Vector of squares.
-- Vector is used instead of List for efficient lookup by square.
type Board = V.Vector (Maybe Piece)

-- |Reads a PieceType given a Char.  This function is case insensitive.
readPieceType :: Char -> Maybe PieceType
readPieceType c = case toLower c of
  'k' -> Just King
  'q' -> Just Queen
  'r' -> Just Rook
  'b' -> Just Bishop
  'n' -> Just Knight
  'p' -> Just Pawn
  _ -> Nothing

-- |Shows a square in file-rank format.
-- For example, passing in 28 returns "e4".
showSquare :: Square -> String
showSquare s = f:r where
  f = chr . (+ ord 'a') . file $ s
  r = show . succ . rank $ s

-- |Reads a square in file-rank format.
-- For example, passing in "e4" returns 28.  If an invalid string is passed in, -1 is returned.
readSquare :: String -> Square
readSquare [] = -1
readSquare (f:r) 
  | isNumber . head $ r = 
    ord f - ord 'a' + ((*8) . pred . read $ r)
  | otherwise = -1

-- |Converts Pieces to a printable String.
prettyPieces :: Pieces -> String
prettyPieces = prettyBoard . toBoard
  
-- |Converts Board to a printable String.
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

-- |Converts a Pieces to a Board.
-- This method sorts the passed in Pieces by square and then traverses the List of Pieces to fill the Board vector.
toBoard :: Pieces -> Board
toBoard [] = V.fromList $ replicate 64 Nothing
toBoard ps = V.fromList $ pad (-1) $ foldr combine (64, []) sortedPieces where
  combine p@Piece {square = sq} acc = (sq, Just p:pad sq acc)
  pad curSq (lastSq, acc) =  replicate (lastSq-curSq-1) Nothing ++ acc
  sortedPieces = sortBy comparingSquare ps

-- |Compares pieces by their squares.
comparingSquare :: Piece -> Piece -> Ordering
comparingSquare p1 p2 = compare (square p1) (square p2)
  
-- |Returns the file of a square.
file :: Square -> Int
file n = n .&. 7

-- |Returns the rank of a square.
rank :: Square -> Int
rank n = n `shiftR` 3

-- |Returns the piece occupying a given square on a board, if any.
occupant :: Board -> Square -> Maybe Piece
occupant b sq = b V.! sq

-- |Reverses a PieceColor.
reverseColor :: PieceColor -> PieceColor
reverseColor White = Black
reverseColor _ = White

-- |List of piece types that pawns are allowed to promote to.
promotablePieces :: [PieceType]
promotablePieces = [Knight, Bishop, Rook, Queen]

-- |The initial pieces of a chess game.
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

-- |Empty list of Pieces.
emptyPieces :: Pieces
emptyPieces = []

-- |Sample position, white to move and win.
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

-- |Sicilian Dragon opening.
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
  
-- |A stalemate.
stalemate :: Pieces
stalemate = [
  Piece King Black   (readSquare "a8"),
  Piece Bishop Black (readSquare "b8"),
  Piece Rook White   (readSquare "h8"),
  Piece King White   (readSquare "b6")]
