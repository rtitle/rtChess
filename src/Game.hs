{- |
Game.hs
Contains functionality for representing the state of a chess game.
-}
module Game(
Game(..),
getProhibitedCastles,
newGame,
playGame
) where

import Control.Applicative
import Control.Monad.RWS
import Board
import Moves
import Utils

{- |
  A Game consists of:
  - the Pieces
  - the Board
  - whose turn it is (the PieceColor)
  - list of prohibited castles (initially empty)
  - optional piece eligible for en passant capture
-}
data Game = Game { 
  pieces :: Pieces,
  board :: Board,
  turn :: PieceColor,
  prohibitedCastles :: [(PieceColor, Castle)],
  enPassant :: Maybe Piece }
  
-- |Use a Reader-Writer-State monad to represent the game.
--  Reader is not currently used; Writer is a list of moves as Strings; State is the Game data type. 
type MonadStack = RWS () [String] Game

-- |Gets the list of prohibited castles from a Game.
getProhibitedCastles :: Game -> [Castle]
getProhibitedCastles g = map snd $ filter (\c -> turn g == fst c) (prohibitedCastles g)

-- |Makes a Move and updates the MonadStack.
makeMove :: Move -> MonadStack ()
makeMove move = do
  (Game ps b t prohibCas _) <- get
  -- log the move to the Writer monad.
  tell [showMove b ps t move]
  -- put the new game state to the State monad.
  put $ Game (updatePieces ps move) (updateBoard b move) (reverseColor t) (updateProhibitedCastles ++ prohibCas) getEp
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
    -- if a Pawn has just moved 2 spaces, it is eligble for an en passant capture.
    getEp 
      | (&&&&) <$> (isPawn) <*> (isColor White) <*> (isFromRank 1) <*> (isToRank 3) $ move = Just . toPiece $ move
      | (&&&&) <$> (isPawn) <*> (isColor Black) <*> (isFromRank 6) <*> (isToRank 4) $ move = Just . toPiece $ move
      | otherwise = Nothing
    isPawn m = (pieceType . fromPiece $ m) == Pawn
    isColor c m = (pieceColor . fromPiece $ m) == c
    isFromRank r m = (rank . square . fromPiece $ m) == r
    isToRank r m = (rank . square . toPiece $ m) == r

-- |Starts a new game.
newGame :: Game
newGame = Game initialPieces (toBoard initialPieces) White [] Nothing

-- |Applies the given list of moves to a game.
--  If an invalid move is encounter, the function returns and no subsequent moves are made.
playGame :: [String] -> MonadStack (Either String ())
playGame [] = return $ Right ()
playGame (x:xs) = do
  g@(Game ps b t _ ep) <- get
  let maybeM = readMove b ps t ep (getProhibitedCastles g) x
  case maybeM of
    Just m -> do
      _ <- makeMove m
      playGame xs
    _ -> return . Left $ "Invalid move: " ++ x