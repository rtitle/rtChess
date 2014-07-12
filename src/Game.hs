module Game where
import qualified Data.Vector as V
import Control.Monad.RWS
import Board
import Moves

data Game = Game { 
  pieces :: Pieces,
  board :: Board,
  turn :: PieceColor }
                   
type MonadStack = RWS () [String] Game
                             
makeMove :: Move -> MonadStack ()
makeMove move = do
  (Game ps b t) <- get
  tell [showMove b ps t move]
  put $ Game (updatePieces ps move) (updateBoard b move) (reverseColor t)

updatePieces :: Pieces -> Move -> Pieces
updatePieces ps (Move p1 _ _ (Just cas)) = foldl updatePieces ps (concatCastleMoves (pieceColor p1) cas)
updatePieces ps (Move p1 p2 Nothing _) = p2 : filter (/=p1) ps
updatePieces ps (Move p1 p2 (Just c) _) = p2 : filter (liftM2 (&&) (/=p1) (/=c)) ps

updateBoard :: Board -> Move -> Board
updateBoard b (Move p1 _ _ (Just cas)) = foldl updateBoard b (concatCastleMoves (pieceColor p1) cas)
updateBoard b (Move p1 p2 Nothing _) = b V.// [(square p1, Nothing), (square p2, Just p2)]
updateBoard b (Move p1 p2 (Just c) _) = b V.// [(square p1, Nothing), (square p2, Just p2), (square c, Just p2)]

newGame :: Game
newGame = Game initialPieces (toBoard initialPieces) White

playGame :: [String] -> MonadStack (Either String ())
playGame [] = return $ Right ()
playGame (x:xs) = do
  (Game ps b t) <- get
  let maybeM = readMove b ps t x
  case maybeM of
    Just m -> do
      _ <- makeMove m
      playGame xs
    _ -> return . Left $ "Invalid move: " ++ x