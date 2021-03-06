{- |
Game.hs
Contains functionality for representing the state of a chess game.
-}
module Game(
Position(..),
getProhibitedCastles,
initialPosition,
playMove
) where

import Data.List
import Data.Ord
import Control.Applicative
import Control.Monad.RWS
import Board
import Moves
import Utils
import Eval
import MoveOrdering

data Position = Position { 
  pieces :: Pieces,
  board :: Board,
  turn :: PieceColor,
  lastMove :: Maybe Move,
  prohibitedCastles :: [(PieceColor, Castle)],
  enPassant :: Maybe Piece
}

data PositionTree = PositionTree {
  position :: Position,
  positionTree :: [PositionTree]
}

data GameState = Playing | Checkmate | Stalemate

type Game = RWS () [String] Position

getProhibitedCastles :: Position -> [Castle]
getProhibitedCastles p = map snd $ filter (\c -> turn p == fst c) (prohibitedCastles p)

makeMove' :: Move -> Game ()
makeMove' move = do
  -- get the current position from the State monad.
  p@(Position ps b t _ _ _) <- get
  -- log the move to the Writer monad.
  tell [showMove b ps t move]
  -- put the new position to the State monad.
  let p' = makeMove p move
  put p'
  -- check for checkmate or stalemate
  case (checkGameState p') of
    Checkmate -> tell [winner t]
    Stalemate -> tell [draw]
    Playing -> return ()
    
makeMove :: Position -> Move -> Position
makeMove (Position ps b t _ prohibCas _) move = Position (updatePieces ps move) (updateBoard b move) (reverseColor t) (Just move) (updateProhibitedCastles ++ prohibCas) getEp
  where
    -- if a King or a Rook moves from their original squares, update list of prohibited castles.
    updateProhibitedCastles 
      | fromPiece move == Piece Rook White (readSquare "a1") = [(White, Queenside)]
      | fromPiece move == Piece Rook White (readSquare "h1") = [(White, Kingside)]
      | fromPiece move == Piece King White (readSquare "e1") = [(White, Queenside), (White, Kingside)]
      | fromPiece move == Piece Rook Black (readSquare "a8") = [(Black, Queenside)]
      | fromPiece move == Piece Rook Black (readSquare "h8") = [(Black, Kingside)]
      | fromPiece move == Piece King Black (readSquare "e8") = [(Black, Queenside), (White, Kingside)]
      | otherwise = []
    -- if a Pawn has just moved 2 spaces, it is eligible for an en passant capture.
    getEp 
      | (&&&&) <$> (isPawn) <*> (isColor White) <*> (isFromRank 1) <*> (isToRank 3) $ move = Just . toPiece $ move
      | (&&&&) <$> (isPawn) <*> (isColor Black) <*> (isFromRank 6) <*> (isToRank 4) $ move = Just . toPiece $ move
      | otherwise = Nothing
    isPawn m = (pieceType . fromPiece $ m) == Pawn
    isColor c m = (pieceColor . fromPiece $ m) == c
    isFromRank r m = (rank . square . fromPiece $ m) == r
    isToRank r m = (rank . square . toPiece $ m) == r

initialPosition :: Position
initialPosition = Position initialPieces (toBoard initialPieces) White Nothing [] Nothing
    
playMove :: String -> Game (Either String Move)
playMove s = do
  p@(Position ps b t _ _ ep) <- get
  case (checkGameState p) of
    Playing -> do
      let maybeM = readMove b ps t ep (getProhibitedCastles p) s
      case maybeM of
        Just m -> do
          _ <- makeMove' m
          p' <- get
          case (checkGameState p') of
            Playing -> do
              let maybeComputerM = findBestMove p'
              case maybeComputerM of
                Just cm -> do
                  _ <- makeMove' cm
                  return . Right $ cm
                Nothing -> return . Left $ "No best moves found :("
            Checkmate -> do 
              return . Left $ "Checkmate! Congratulations!"
            Stalemate -> do
              return . Left $ "Stalemate. Ha-ha."
        Nothing -> return . Left $ "Invalid move: " ++ s
    _ -> return . Left $ "The game is over."  

checkGameState :: Position -> GameState
checkGameState p@(Position ps b t _ _ ep) = case (legalMovesNotInCheck b ps t ep (getProhibitedCastles p)) of
  [] -> case (isKingInCheck b ps t) of
    True -> Checkmate
    False -> Stalemate
  _ -> Playing
    
winner :: PieceColor -> String
winner White = "1 - 0"
winner Black = "0 - 1"

draw :: String
draw = "1/2 - 1/2"
    
genGameTree :: Int -> Position -> PositionTree
genGameTree 0 p = PositionTree p []
genGameTree depth p@(Position ps b t _ _ ep)
  | finalMove p = PositionTree p []
  | otherwise = PositionTree p (map (genGameTree (depth-1)) nextMoves)
  where 
    nextMoves = map (makeMove p) (sortBy (comparingMoveReverse b ps) (legalMoves b ps t ep (getProhibitedCastles p)))

negamax :: Int -> Int -> PositionTree -> Int
negamax _ _ (PositionTree (Position ps _ _ _ _ _) []) = evalBoard ps
negamax a b (PositionTree p (x:xs)) = prune a b (negamax (-b) (-a) x) xs
  where prune a' b' e es
          | b' < e = -e
          | null es = -max a' e
          | otherwise = negamax (max a' e) b' (PositionTree p es)
  
findBestMove :: Position -> (Maybe Move)
findBestMove p = let (PositionTree _ xs) = genGameTree 4 p in
  lastMove . position $ maximumBy (comparing $ negamax (-infinity) (infinity)) xs
  
finalMove :: Position -> Bool
finalMove (Position ps _ _ _ _ _) = e > threshold || e < -threshold
  where e = evalBoard ps

