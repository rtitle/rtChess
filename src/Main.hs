module Main where
import Control.Monad.State
import Board
import Game

main::IO()
main = do
  putStrLn $ "New Game:"
  putStrLn $ prettyPieces initialPieces
  putStrLn $ "Making the following moves: " ++ show moves
  let (m,g) = runState (playGame moves) newGame
  putStrLn $ prettyPieces . pieces $ g
  putStrLn $ "Possible moves for " ++ show (turn g) ++ ": " ++ show m
  where moves = ["e4", "e5", "Nf3", "Nc6", "Bb5", "a6", "Bxc6", "dxc6", "Nxe5", "Qd4"]