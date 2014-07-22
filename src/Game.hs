module Game where
import Control.Monad.RWS
import Board
import Moves

data Game = Game { 
  pieces :: Pieces,
  board :: Board,
  turn :: PieceColor,
  prohibitedCastles :: [(PieceColor, Castle)] }
                   
type MonadStack = RWS () [String] Game

getProhibitedCastles :: [(PieceColor, Castle)] -> PieceColor -> [Castle]
getProhibitedCastles p t = map snd $ filter (\c -> t == fst c) p
                        
makeMove :: Move -> MonadStack ()
makeMove move = do
  (Game ps b t prohibCas) <- get
  tell [showMove b ps t move]
  put $ Game (updatePieces ps move) (updateBoard b move) (reverseColor t) (updateWhiteProhibitedCastles ++ prohibCas)
  where 
    updateWhiteProhibitedCastles 
      | fromPiece move == Piece Rook White (readSquare "a1") = [(White, Queenside)]
      | fromPiece move == Piece Rook White (readSquare "h1") = [(White, Kingside)]
      | fromPiece move == Piece King White (readSquare "e1") = [(White, Queenside), (White, Kingside)]
      | fromPiece move == Piece Rook Black (readSquare "a8") = [(Black, Queenside)]
      | fromPiece move == Piece Rook Black (readSquare "h8") = [(Black, Kingside)]
      | fromPiece move == Piece King Black (readSquare "e8") = [(Black, Queenside), (White, Kingside)]
      | otherwise = []

newGame :: Game
newGame = Game initialPieces (toBoard initialPieces) White []

playGame :: [String] -> MonadStack (Either String ())
playGame [] = return $ Right ()
playGame (x:xs) = do
  (Game ps b t prohibCas) <- get
  let maybeM = readMove b ps t (getProhibitedCastles prohibCas t) x
  case maybeM of
    Just m -> do
      _ <- makeMove m
      playGame xs
    _ -> return . Left $ "Invalid move: " ++ x