module Game where
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