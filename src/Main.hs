module Main where
import Control.Monad.State
import Board
import Game

getMove :: IO String
getMove = do
  putStrLn "Enter move:"
  m <- getLine
  return m
  
play :: Game -> IO ()
play game = do
  mv <- getMove
  let (mvs,g) = runState (playGame [mv]) game
  putStrLn $ prettyPieces . pieces $ g
  putStrLn $ show (length mvs) ++ " possible moves for " ++ show (turn g) ++ ": " ++ show mvs
  play g

main :: IO ()
main = do
  putStrLn $ "New Game:"
  putStrLn $ prettyPieces initialPieces
  play newGame