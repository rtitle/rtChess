module Game where
import qualified Data.Vector as V
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.RWS
import Board
import Moves

data Game = Game { pieces :: Pieces,
                   board :: Board,
                   turn :: PieceColor }
                   
type MonadStack = RWS () [String] Game Bool
                             
makeMove :: Move -> MonadStack
makeMove move = do
  (Game ps b t) <- get
  tell [showMove b ps t move]
  put $ Game (updatePieces ps move) (updateBoard b move) (reverseColor t)
  return True

updatePieces :: Pieces -> Move -> Pieces
updatePieces ps (Move p1 p2 Nothing) = p2 : filter (/=p1) ps
updatePieces ps (Move p1 p2 (Just c)) = p2 : filter (liftM2 (&&) (/=p1) (/=c)) ps

updateBoard :: Board -> Move -> Board
updateBoard b (Move p1 p2 Nothing) = b V.// [(square p1, Nothing), (square p2, Just p2)]
updateBoard b (Move p1 p2 (Just c)) = b V.// [(square p1, Nothing), (square p2, Just p2), (square c, Just p2)]

newGame :: Game
newGame = Game initialPieces (toBoard initialPieces) White

playGame :: [String] -> Bool -> MonadStack
playGame [] acc = return acc
playGame (x:xs) acc = do
  (Game ps b t) <- get
  let maybeM = readMove b ps t x
  case maybeM of
    Just m -> do
      _ <- makeMove m
      playGame xs acc
    _ -> return False