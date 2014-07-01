module Board where
import Data.Char
import Data.List

-- ** data types **

type Square = Int
data PieceType = Rook | Knight | Bishop | King | Queen | Pawn deriving Eq
data PieceColor = Black | White deriving Eq
data Piece = Piece { pieceType::PieceType, 
                     pieceColor::PieceColor,
                     square::Square } deriving Eq          
-- the board is represented by an array of pieces
type Board = [Piece]
 
-- ** type class instances **

instance Show PieceType where
  show King = "K"
  show Queen = "Q"
  show Knight = "N"
  show Rook = "R"
  show Bishop = "B"
  show Pawn = "P"
  
instance Show Piece where
  -- black pieces are lowercase
  show (Piece pt Black _) = [toLower $ head $ show pt]
  show (Piece pt White _) = [head $ show $ pt]
  
-- ** output functions **

showSquare::Square -> String
showSquare s = file:rank where
  file = (chr . (+ ord 'a') . mod8) s
  rank = (show . succ . quot8) s

readSquare::String -> Square
readSquare [] = -1
readSquare (file:rank) = ord file - ord 'a' +
                         (mult8 . pred . read) rank

prettyBoard::Board -> String
prettyBoard = unlines . 
             (hline++) . 
             concatMap (:hline) . 
             reverse . 
             map (++vline) . 
             map concat . 
             splitEvery 8 . 
             map (vline++) .
             map prettySquare . 
             toSquareBoard where
  hline = [vline ++ (concat . replicate 31) "-" ++ vline]
  vline = "|"
  prettySquare Nothing = "   "
  prettySquare (Just p) = " " ++ show p ++ " "

toSquareBoard::Board -> [Maybe Piece]
toSquareBoard [] = replicate 64 Nothing
toSquareBoard b = pad 64 $ foldl combine (-1, []) sortedBoard where
  combine acc (Piece pt col sq) = (sq, pad sq acc ++ [Just (Piece pt col sq)])
  pad curSq (lastSq, acc) =  acc ++ (replicate (curSq-lastSq-1) Nothing)
  sortedBoard = sortBy comparingSquare b

-- ** auxiliary helper functions **

comparingSquare::Piece -> Piece -> Ordering
comparingSquare (Piece _ _ sq1) (Piece _ _ sq2)
  | sq1 < sq2 = LT
  | sq1 > sq2 = GT
  | otherwise = EQ
  
mod8::Int -> Int
mod8 n = mod n 8

quot8::Int -> Int
quot8 n = quot n 8

mult8::Int -> Int
mult8 n = 8 * n

splitEvery :: Int -> [a] -> [[a]]
splitEvery n = takeWhile (not . null) . map (take n) . iterate (drop n)

-- ** some boards **

initialBoard::Board
initialBoard = [
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
  
emptyBoard::Board
emptyBoard = []

mateIn1::Board
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

sicilianDragon::Board
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
