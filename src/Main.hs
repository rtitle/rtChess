module Main where
import Board
import Game
import Control.Monad.State

main::IO()
main = do
  putStrLn $ "New Game:"
  putStrLn $ prettyPieces initialPieces
  putStrLn $ "Making "++show n++" random moves.  Resulting position:"
  putStrLn $ prettyPieces (evalState (playGame n) newGame)
  where n = 25