module Game where
import Control.Monad.State
import Board
import Moves
import qualified Data.Vector as V

data Game = Game { pieces :: Pieces,
                   board :: Board,
                   turn :: PieceColor }
                             
makeMove :: Move -> State Game ()
makeMove move = do
               (Game ps b t) <- get
               put $ Game (updatePieces ps move) (updateBoard b move) (reverseColor t)
               
readMove :: Game -> String -> Move
readMove (Game ps b t) s = 
  let toSq = readSquare . reverse . take 2 . reverse $ s
      pt = readPieceType . head $ s
      fromFile = case (pt, occupant b toSq) of 
                   (Pawn, (Just _)) -> Just $ head s
                   _ -> Nothing
      f m = toSq == (square . toPiece $ m) && 
            pt == (pieceType . toPiece $ m) &&
            (fromFile == Nothing || fromFile == Just (head . showSquare . square . fromPiece $ m))
  in head $ filter f (allMoves b ps t)

updatePieces :: Pieces -> Move -> Pieces
updatePieces ps (Move p1 p2 Nothing) = p2 : filter (/=p1) ps
updatePieces ps (Move p1 p2 (Just c)) = p2 : filter (liftM2 (&&) (/=p1) (/=c)) ps

updateBoard :: Board -> Move -> Board
updateBoard b (Move p1 p2 Nothing) = b V.// [(square p1, Nothing), (square p2, Just p2)]
updateBoard b (Move p1 p2 (Just c)) = b V.// [(square p1, Nothing), (square p2, Just p2), (square c, Just p2)]

newGame :: Game
newGame = Game initialPieces (toBoard initialPieces) White

playGame :: [String] -> State Game [Move]
playGame [] = do
              (Game ps b t) <- get
              return $ allMoves b ps t
playGame (x:xs) = do
                  g <- get
                  makeMove $ readMove g x
                  playGame xs
