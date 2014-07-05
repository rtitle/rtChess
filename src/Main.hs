module Main where
import Board
import Moves
import Game
import Control.Monad.State

main::IO()
main = do
  putStrLn $ "New Game:"
  putStrLn $ prettyPieces initialPieces
  putStrLn $ "Making "++show n++" random moves.  Resulting position and moves: "
  let (Game ps b c) = execState (playGame n) newGame
  putStrLn $ prettyPieces ps
  putStrLn $ show (allMoves b ps c)
  where n = 20